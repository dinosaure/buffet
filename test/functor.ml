module type RD = sig
  type t

  include Buffet.S.S0 with type t := t
end

external ( = ) : 'a -> 'a -> bool = "%equal"

let equal (x : int) y = x = y

module Make (Buffer : RD) = struct
  let start_search pattern max_i text =
    let hash = ref 0 in
    let identical = ref true in
    for pos = 0 to max_i do
      let pattern_char = (Buffer.unsafe_get_uint8 pattern pos :> int) in
      let text_char = (Buffer.unsafe_get_uint8 text pos :> int) in
      hash := !hash + text_char - pattern_char ;
      identical := !identical && equal pattern_char text_char
    done ;
    (!hash, !identical)

  let none = None

  exception Found

  let search pattern max_i text max_pos hash pos =
    let hash = ref hash in
    let pos = ref (pos + 1) in
    let i = ref 0 in
    try
      while !pos <= max_pos do
        hash :=
          !hash
          - ( (Buffer.unsafe_get_uint8 text (!pos - 1) :> int)
            + (Buffer.unsafe_get_uint8 text (!pos + max_i) :> int) ) ;
        if !hash = 0 then (
          i := 0 ;
          while
            if !i = max_i then raise Found ;
            (equal : int -> int -> bool)
              (Buffer.unsafe_get_uint8 text (!pos + !i) :> int)
              (Buffer.unsafe_get_uint8 pattern !i :> int)
          do
            incr i
          done ) ;
        incr pos
      done ;
      none
    with Found -> Some !pos

  let find_all pattern text =
    let max_i = Buffer.length pattern - 1 in
    let max_pos = Buffer.length text - max_i - 1 in
    let hash, identical = start_search pattern max_i text in
    let count = if identical then 1 else 0 in
    let rec go count hash pos =
      match search pattern max_i text max_pos hash pos with
      | Some pos -> go (count + 1) 0 pos
      | None -> count
    in
    go count hash 0
end

module X0 = Make (Buffet.Buffet0.String)
module X1 = Make (Buffet.Buffet0.Bytes)
module X2 = Make (Buffet.Buffet0.Bigstring)

module RK = struct
  let start_search ~witness pattern max_i text =
    let hash = ref 0 in
    let identical = ref true in
    for pos = 0 to max_i do
      let pattern_char =
        (Buffet.Buffet1.unsafe_get_uint8 witness pattern pos :> int)
      in
      let text_char =
        (Buffet.Buffet1.unsafe_get_uint8 witness text pos :> int)
      in
      hash := !hash + text_char - pattern_char ;
      identical := !identical && equal pattern_char text_char
    done ;
    (!hash, !identical)

  let none = None

  exception Found

  let search ~witness pattern max_i text max_pos hash pos =
    let hash = ref hash in
    let pos = ref (pos + 1) in
    let i = ref 0 in
    try
      while !pos <= max_pos do
        hash :=
          !hash
          - ( ( (Buffet.Buffet1.unsafe_get_uint8 [@inlined always]) witness
                  text (!pos - 1)
                :> int )
            + ( (Buffet.Buffet1.unsafe_get_uint8 [@inlined always]) witness
                  text (!pos + max_i)
                :> int ) ) ;
        if !hash = 0 then (
          i := 0 ;
          while
            if !i = max_i then raise Found ;
            (equal : int -> int -> bool)
              ( (Buffet.Buffet1.unsafe_get_uint8 [@inlined always]) witness
                  text (!pos + !i)
                :> int )
              ( (Buffet.Buffet1.unsafe_get_uint8 [@inlined always]) witness
                  pattern !i
                :> int )
          do
            incr i
          done ) ;
        incr pos
      done ;
      none
    with Found -> Some !pos

  let find_all ~witness pattern text =
    let max_i = Buffet.Buffet1.length witness pattern - 1 in
    let max_pos = Buffet.Buffet1.length witness text - max_i - 1 in
    let hash, identical = start_search ~witness pattern max_i text in
    let count = if identical then 1 else 0 in
    let rec go count hash pos =
      match search ~witness pattern max_i text max_pos hash pos with
      | Some pos -> go (count + 1) 0 pos
      | None -> count
    in
    go count hash 0
end

let random_string len =
  let ic = open_in "/dev/urandom" in
  let rs = Bytes.create len in
  really_input ic rs 0 len ; close_in ic ; Bytes.unsafe_to_string rs

open Bechamel
open Toolkit

module Monotonic_clock = struct
  type witness = int
  type value = int64 ref
  type label = string

  let make () = Oclock.monotonic
  let load _witness = ()
  let unload _witness = ()
  let float v = Int64.to_float !v
  let label _witness = "monotonic-clock"
  let diff a b = {contents= Int64.sub !b !a}
  let epsilon () = {contents= 0L}
  let blit witness v = v := Oclock.gettime witness
end

module Extension = struct
  include Extension

  let monotonic_clock = Measure.make (module Monotonic_clock)
end

module Instance = struct
  include Instance

  let monotonic_clock =
    Measure.instance (module Monotonic_clock) Extension.monotonic_clock
end

let text_string = random_string (4096 * 2)
let pattern_string = random_string 128
let text_bytes = Bytes.unsafe_of_string text_string
let pattern_bytes = Bytes.unsafe_of_string pattern_string

let test_buffet_0_on_string =
  Staged.stage (fun () -> X0.find_all pattern_string text_string)

let test_buffet_0_on_bytes =
  Staged.stage (fun () -> X1.find_all pattern_bytes text_bytes)

let test_buffet_1_on_string =
  Staged.stage (fun () ->
      (RK.find_all ~witness:Buffet.Buffet1.string [@specialized always])
        pattern_string text_string )

let test_buffet_1_on_bytes =
  Staged.stage (fun () ->
      (RK.find_all ~witness:Buffet.Buffet1.bytes [@specialized always])
        pattern_bytes text_bytes )

let tests =
  [ Test.make ~name:"buffet0:string" test_buffet_0_on_string
  ; Test.make ~name:"buffet0:bytes" test_buffet_0_on_bytes
  ; Test.make ~name:"buffet1:string" test_buffet_1_on_string
  ; Test.make ~name:"buffet1:bytes" test_buffet_1_on_bytes ]

let zip l1 l2 =
  let rec go acc = function
    | [], [] -> List.rev acc
    | x1 :: r1, x2 :: r2 -> go ((x1, x2) :: acc) (r1, r2)
    | _, _ -> assert false
  in
  go [] (l1, l2)

let pp_result ppf result =
  let style_by_r_square =
    match Analyze.OLS.r_square result with
    | Some r_square ->
        if r_square >= 0.95 then `Green
        else if r_square >= 0.90 then `Yellow
        else `Red
    | None -> `None
  in
  match Analyze.OLS.estimates result with
  | Some estimates ->
      Fmt.pf ppf "%a per %a = %a" Label.pp
        (Analyze.OLS.responder result)
        Fmt.(Dump.list Label.pp)
        (Analyze.OLS.predictors result)
        Fmt.(styled style_by_r_square (Dump.list float))
        estimates
  | None ->
      Fmt.pf ppf "%a per %a = #unable-to-compute" Label.pp
        (Analyze.OLS.responder result)
        Fmt.(Dump.list Label.pp)
        (Analyze.OLS.predictors result)

let pad n x =
  if String.length x > n then x else x ^ String.make (n - String.length x) ' '

let pp ppf (test, results) =
  let tests = Test.set test in
  List.iter
    (fun results ->
      List.iter
        (fun (test, result) ->
          Fmt.pf ppf "@[<hov>%s = %a@]@\n"
            (pad 30 @@ Test.Elt.name test)
            pp_result result )
        (zip tests results) )
    results

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over () ; k () in
    let with_src_and_stamp h _ k fmt =
      let dt = Mtime.Span.to_us (Mtime_clock.elapsed ()) in
      Fmt.kpf k ppf
        ("%s %a %a: @[" ^^ fmt ^^ "@]@.")
        (pad 20 (Fmt.strf "%+04.0fus" dt))
        Logs_fmt.pp_header (level, h)
        Fmt.(styled `Magenta string)
        (pad 20 @@ Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_src_and_stamp header tags k fmt
  in
  {Logs.report}

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stdout) ;
  let quiet = match style_renderer with Some _ -> true | None -> false in
  (quiet, Fmt.stdout)

let _, _ = setup_logs (Some `Ansi_tty) (Some Logs.Debug)

let () =
  let ols =
    Analyze.ols ~r_square:true ~bootstrap:0 ~predictors:Measure.[|run|]
  in
  let instances = Instance.[monotonic_clock] in
  let measure_and_analyze test =
    let results =
      Benchmark.all ~stabilize:true ~quota:(Benchmark.s 1.) ~run:3000 instances
        test
    in
    List.map
      (fun x -> List.map (Analyze.analyze ols (Measure.label x)) results)
      instances
  in
  let results = List.map measure_and_analyze tests in
  List.iter
    (fun (test, result) ->
      Fmt.pr "---------- %s ----------\n%!" (Test.name test) ;
      Fmt.pr "%a\n%!" pp (test, result) )
    (zip tests results)
