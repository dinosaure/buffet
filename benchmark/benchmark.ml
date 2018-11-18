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

module Realtime_clock = struct
  type witness = int
  type value = int64 ref
  type label = string

  let make () = Oclock.realtime
  let load _witness = ()
  let unload _witness = ()
  let float v = Int64.to_float !v
  let label _witness = "realtime-clock"
  let diff a b = {contents= Int64.sub !b !a}
  let epsilon () = {contents= 0L}
  let blit witness v = v := Oclock.gettime witness
end

module Extension = struct
  include Extension

  let monotonic_clock = Measure.make (module Monotonic_clock)
  let realtime_clock = Measure.make (module Realtime_clock)
end

module Instance = struct
  include Instance

  let monotonic_clock =
    Measure.instance (module Monotonic_clock) Extension.monotonic_clock

  let realtime_clock =
    Measure.instance (module Realtime_clock) Extension.realtime_clock
end

(** TESTS **)

let () = Random.self_init ()

let create_buffet_0_bytes len =
  Staged.stage (fun () -> Buffet.Buffet0.Bytes.create len)

let create_buffet_0_bigstring len =
  Staged.stage (fun () -> Buffet.Buffet0.Bigstring.create len)

let set_buffet_0_bytes len =
  let buf = Buffet.Buffet0.Bytes.create len in
  let pos = Random.int len in
  Staged.stage (fun () -> Buffet.Buffet0.Bytes.set buf pos '\042')

let set_buffet_0_bigstring len =
  let buf = Buffet.Buffet0.Bigstring.create len in
  let pos = Random.int len in
  Staged.stage (fun () -> Buffet.Buffet0.Bigstring.set buf pos '\042')

let create_buffet_1_bytes len =
  Staged.stage (fun () -> Buffet.Buffet1.(create bytes len))

let create_buffet_1_bigstring len =
  Staged.stage (fun () -> Buffet.Buffet1.(create bigstring len))

let set_buffet_1_bytes len =
  let buf = Buffet.Buffet1.(create bytes len) in
  let pos = Random.int len in
  Staged.stage (fun () -> Buffet.Buffet1.(set bytes buf pos '\042'))

let set_buffet_1_bigstring len =
  let buf = Buffet.Buffet1.(create bigstring len) in
  let pos = Random.int len in
  Staged.stage (fun () -> Buffet.Buffet1.(set bigstring buf pos '\042'))

let unsafe_set_buffet_0_bytes len =
  let buf = Buffet.Buffet0.Bytes.create len in
  let pos = Random.int len in
  Staged.stage (fun () -> Buffet.Buffet0.Bytes.unsafe_set buf pos '\042')

let unsafe_set_buffet_0_bigstring len =
  let buf = Buffet.Buffet0.Bigstring.create len in
  let pos = Random.int len in
  Staged.stage (fun () -> Buffet.Buffet0.Bigstring.unsafe_set buf pos '\042')

let unsafe_set_buffet_1_bytes len =
  let buf = Buffet.Buffet1.(create bytes len) in
  let pos = Random.int len in
  Staged.stage (fun () -> Buffet.Buffet1.(unsafe_set bytes buf pos '\042'))

let unsafe_set_buffet_1_bigstring len =
  let buf = Buffet.Buffet1.(create bigstring len) in
  let pos = Random.int len in
  Staged.stage (fun () -> Buffet.Buffet1.(unsafe_set bigstring buf pos '\042'))

let create_bigstringaf len = Staged.stage (fun () -> Bigstringaf.create len)

let create_buffet_2_bytes len =
  Staged.stage (fun () -> Buffet.Buffet2.(create bytes len))

let create_buffet_2_bigstring len =
  Staged.stage (fun () -> Buffet.Buffet2.(create bigstring len))

let create_buffet_6_bytes len =
  Staged.stage (fun () -> Buffet.Buffet6.Bytes.create len)

let create_buffet_6_bigstring len =
  Staged.stage (fun () -> Buffet.Buffet6.Bigstring.create len)

let set_buffet_6_bytes len =
  let buf = Buffet.Buffet6.Bytes.create len in
  let pos = Random.int len in
  Staged.stage (fun () -> Buffet.Buffet6.Bytes.set buf pos '\042')

let set_buffet_6_bigstring len =
  let buf = Buffet.Buffet6.Bigstring.create len in
  let pos = Random.int len in
  Staged.stage (fun () -> Buffet.Buffet6.Bigstring.set buf pos '\042')

let set_buffet_7_bytes len =
  let buf = Buffet.Buffet6.Bytes.create len in
  let buf = Buffet.Buffet7.Injection.bytes buf in
  let pos = Random.int len in
  Staged.stage (fun () -> Buffet.Buffet7.(set bytes buf pos '\042'))

let set_buffet_7_bigstring len =
  let buf = Buffet.Buffet6.Bigstring.create len in
  let buf = Buffet.Buffet7.Injection.bigstring buf in
  let pos = Random.int len in
  Staged.stage (fun () -> Buffet.Buffet7.(set bigstring buf pos '\042'))

let unsafe_set_buffet_7_bytes len =
  let buf = Buffet.Buffet6.Bytes.create len in
  let buf = Buffet.Buffet7.Injection.bytes buf in
  let pos = Random.int len in
  Staged.stage (fun () -> Buffet.Buffet7.(unsafe_set bytes buf pos '\042'))

let unsafe_set_buffet_7_bigstring len =
  let buf = Buffet.Buffet6.Bigstring.create len in
  let buf = Buffet.Buffet7.Injection.bigstring buf in
  let pos = Random.int len in
  Staged.stage (fun () -> Buffet.Buffet7.(unsafe_set bigstring buf pos '\042'))

let sum_buffet_0 ~create ~get len =
  let buf = create len in
  let sum () =
    let r = ref 0 in
    for i = 0 to len - 1 do
      r := Char.code (get buf i) + !r
    done ;
    !r
  in
  Staged.stage sum

let sum_buffet_1 witness len =
  let buf = Buffet.Buffet1.create witness len in
  let sum () =
    let r = ref 0 in
    for i = 0 to len - 1 do
      r := Char.code (Buffet.Buffet1.get witness buf i) + !r
    done ;
    !r
  in
  Staged.stage sum

let sum_buffet_2 witness len =
  let buf = Buffet.Buffet2.create witness len in
  let sum () =
    let r = ref 0 in
    for i = 0 to len - 1 do
      r := Char.code (Buffet.Buffet2.get witness buf i) + !r
    done ;
    !r
  in
  Staged.stage sum

let sum_buffet_7 witness len =
  let buf = Buffet.Buffet7.create witness len in
  let sum () =
    let r = ref 0 in
    for i = 0 to len - 1 do
      r := Char.code (Buffet.Buffet7.get witness buf i) + !r
    done ;
    !r
  in
  Staged.stage sum

let sum_buffet_8 witness len =
  let buf = Buffet.Buffet8.create witness len in
  let sum () =
    let r = ref 0 in
    for i = 0 to len - 1 do
      r := Char.code (Buffet.Buffet8.get witness buf i) + !r
    done ;
    !r
  in
  Staged.stage sum

let sum_buffet_10 witness len =
  let buf = Buffet.Buffet10.create witness len in
  let sum () =
    let r = ref 0 in
    for i = 0 to len - 1 do
      r := Char.code (Buffet.Buffet10.get witness buf i) + !r
    done ;
    !r
  in
  Staged.stage sum

let test_buffet_0_bytes_create =
  Test.make_indexed ~name:"Buffet0.Bytes.create"
    ~args:[0; 1; 10; 100; 500; 1000] create_buffet_0_bytes

let test_buffet_0_bigstring_create =
  Test.make_indexed ~name:"Buffet0.Bigstring.create"
    ~args:[0; 1; 10; 100; 500; 1000] create_buffet_0_bigstring

let test_buffet_1_bytes_create =
  Test.make_indexed ~name:"Buffet1.Bytes.create"
    ~args:[0; 1; 10; 100; 500; 1000] create_buffet_1_bytes

let test_buffet_1_bigstring_create =
  Test.make_indexed ~name:"Buffet1.Bigstring.create"
    ~args:[0; 1; 10; 100; 500; 1000] create_buffet_1_bigstring

let test_buffet_2_bytes_create =
  Test.make_indexed ~name:"Buffet2.Bytes.create"
    ~args:[0; 1; 10; 100; 500; 1000] create_buffet_2_bytes

let test_buffet_2_bigstring_create =
  Test.make_indexed ~name:"Buffet2.Bigstring.create"
    ~args:[0; 1; 10; 100; 500; 1000] create_buffet_2_bigstring

let test_bigstringaf_create =
  Test.make_indexed ~name:"Bigstringaf.create" ~args:[0; 1; 10; 100; 500; 1000]
    create_bigstringaf

let test_buffet_6_bytes_create =
  Test.make_indexed ~name:"Buffet6.Bytes.create"
    ~args:[0; 1; 10; 100; 500; 1000] create_buffet_6_bytes

let test_buffet_6_bigstring_create =
  Test.make_indexed ~name:"Buffet6.Bigstring.create"
    ~args:[0; 1; 10; 100; 500; 1000] create_buffet_6_bigstring

let test_create =
  [ test_bigstringaf_create; test_buffet_0_bytes_create
  ; test_buffet_0_bigstring_create; test_buffet_1_bytes_create
  ; test_buffet_1_bigstring_create; test_buffet_2_bytes_create
  ; test_buffet_2_bigstring_create; test_buffet_6_bytes_create
  ; test_buffet_6_bigstring_create ]

let test_buffet_0_bytes_set =
  Test.make_indexed ~name:"Buffet0.Bytes.set" ~args:[1; 10; 100]
    set_buffet_0_bytes

let test_buffet_0_bigstring_set =
  Test.make_indexed ~name:"Buffet0.Bigstring.set" ~args:[1; 10; 100]
    set_buffet_0_bigstring

let test_buffet_1_bytes_set =
  Test.make_indexed ~name:"Buffet1.Bytes.set" ~args:[1; 10; 100]
    set_buffet_1_bytes

let test_buffet_1_bigstring_set =
  Test.make_indexed ~name:"Buffet1.Bigstring.set" ~args:[1; 10; 100]
    set_buffet_1_bigstring

let test_buffet_6_bytes_set =
  Test.make_indexed ~name:"Buffet6.Bytes.set" ~args:[1; 10; 100]
    set_buffet_6_bytes

let test_buffet_6_bigstring_set =
  Test.make_indexed ~name:"Buffet6.Bigstring.set" ~args:[1; 10; 100]
    set_buffet_6_bigstring

let test_buffet_7_bytes_set =
  Test.make_indexed ~name:"Buffet7.Bytes.set" ~args:[1; 10; 100]
    set_buffet_7_bytes

let test_buffet_7_bigstring_set =
  Test.make_indexed ~name:"Buffet7.Bigstring.set" ~args:[1; 10; 100]
    set_buffet_7_bigstring

let test_set =
  [ test_buffet_0_bytes_set; test_buffet_0_bigstring_set
  ; test_buffet_1_bytes_set; test_buffet_1_bigstring_set
  ; test_buffet_6_bytes_set; test_buffet_6_bigstring_set
  ; test_buffet_7_bytes_set; test_buffet_7_bigstring_set ]

let test_buffet_0_bytes_unsafe_set =
  Test.make_indexed ~name:"Buffet0.Bytes.unsafe_set" ~args:[1; 10; 100]
    unsafe_set_buffet_0_bytes

let test_buffet_0_bigstring_unsafe_set =
  Test.make_indexed ~name:"Buffet0.Bigstring.unsafe_set" ~args:[1; 10; 100]
    unsafe_set_buffet_0_bigstring

let test_buffet_1_bytes_unsafe_set =
  Test.make_indexed ~name:"Buffet1.Bytes.unsafe_set" ~args:[1; 10; 100]
    unsafe_set_buffet_1_bytes

let test_buffet_1_bigstring_unsafe_set =
  Test.make_indexed ~name:"Buffet1.Bigstring.unsafe_set" ~args:[1; 10; 100]
    unsafe_set_buffet_1_bigstring

let test_buffet_7_bytes_unsafe_set =
  Test.make_indexed ~name:"Buffet7.Bytes.unsafe_set" ~args:[1; 10; 100]
    unsafe_set_buffet_7_bytes

let test_buffet_7_bigstring_unsafe_set =
  Test.make_indexed ~name:"Buffet7.Bigstring.unsafe_set" ~args:[1; 10; 100]
    unsafe_set_buffet_7_bigstring

let test_unsafe_set =
  [ test_buffet_0_bytes_unsafe_set; test_buffet_0_bigstring_unsafe_set
  ; test_buffet_1_bytes_unsafe_set; test_buffet_1_bigstring_unsafe_set
  ; test_buffet_7_bytes_unsafe_set; test_buffet_7_bigstring_unsafe_set ]

let test_sum_buffet_0_bytes =
  Test.make_indexed ~name:"Buffet0.Bytes.sum" ~args:[0; 1; 100; 500; 1000]
    (sum_buffet_0 ~create:Buffet.Buffet0.Bytes.create
       ~get:Buffet.Buffet0.Bytes.get)

let test_sum_buffet_0_bigstring =
  Test.make_indexed ~name:"Buffet0.Bigstring.sum" ~args:[0; 1; 100; 500; 1000]
    (sum_buffet_0 ~create:Buffet.Buffet0.Bigstring.create
       ~get:Buffet.Buffet0.Bigstring.get)

let test_sum_buffet_1_bytes =
  Test.make_indexed ~name:"Buffet1.Bytes.sum" ~args:[0; 1; 100; 500; 1000]
    (sum_buffet_1 Buffet.Buffet1.bytes)

let test_sum_buffet_1_bigstring =
  Test.make_indexed ~name:"Buffet1.Bigstring.sum" ~args:[0; 1; 100; 500; 1000]
    (sum_buffet_1 Buffet.Buffet1.bigstring)

let test_sum_buffet_2_bytes =
  Test.make_indexed ~name:"Buffet2.Bytes.sum" ~args:[0; 1; 100; 500; 1000]
    (sum_buffet_2 Buffet.Buffet2.bytes)

let test_sum_buffet_2_bigstring =
  Test.make_indexed ~name:"Buffet2.Bigstring.sum" ~args:[0; 1; 100; 500; 1000]
    (sum_buffet_2 Buffet.Buffet2.bigstring)

let test_sum_buffet_7_bytes =
  Test.make_indexed ~name:"Buffet7.Bytes.sum" ~args:[0; 1; 100; 500; 1000]
    (sum_buffet_7 Buffet.Buffet7.bytes)

let test_sum_buffet_7_bigstring =
  Test.make_indexed ~name:"Buffet7.Bigstring.sum" ~args:[0; 1; 100; 500; 1000]
    (sum_buffet_7 Buffet.Buffet7.bigstring)

let test_sum_buffet_8_bytes =
  Test.make_indexed ~name:"Buffet8.Bytes.sum" ~args:[0; 1; 100; 500; 1000]
    (sum_buffet_8 Buffet.Buffet8.bytes)

let test_sum_buffet_8_bigstring =
  Test.make_indexed ~name:"Buffet8.Bigstring.sum" ~args:[0; 1; 100; 500; 1000]
    (sum_buffet_8 Buffet.Buffet8.bigstring)

let test_sum_buffet_10_bytes =
  Test.make_indexed ~name:"Buffet10.Bytes.sum" ~args:[0; 1; 100; 500; 1000]
    (sum_buffet_10 Buffet.Buffet10.bytes)

let test_sum_buffet_10_bigstring =
  Test.make_indexed ~name:"Buffet10.Bigstring.sum" ~args:[0; 1; 100; 500; 1000]
    (sum_buffet_10 Buffet.Buffet10.bigstring)

let test_sum =
  [ test_sum_buffet_0_bytes; test_sum_buffet_0_bigstring
  ; test_sum_buffet_1_bytes; test_sum_buffet_1_bigstring
  ; test_sum_buffet_2_bytes; test_sum_buffet_2_bigstring
  ; test_sum_buffet_7_bytes; test_sum_buffet_7_bigstring
  ; test_sum_buffet_8_bytes; test_sum_buffet_8_bigstring
  ; test_sum_buffet_10_bytes; test_sum_buffet_10_bigstring ]

(** TESTS **)

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
  let instances =
    Instance.
      [minor_allocated; major_allocated; monotonic_clock; realtime_clock]
  in
  let tests =
    match Sys.argv with
    | [|_|] -> []
    | [|_; "create"|] -> test_create
    | [|_; "set"|] -> test_set
    | [|_; "unsafe_set"|] -> test_unsafe_set
    | [|_; "sum"|] -> test_sum
    | [|_; "all"|] -> test_create @ test_set @ test_unsafe_set @ test_sum
    | _ -> Fmt.invalid_arg "%s {create|set|unsafe_set|all}" Sys.argv.(1)
  in
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
