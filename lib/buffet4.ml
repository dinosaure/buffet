open Buffet0
open Buffet2
open Buffet3

let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

let invalid_bounds off len real_len =
  invalid_arg "Invalid bounds on buffer (len: %d): off:%d, len:%d" real_len off
    len

type t = V : (('rd, 'wr, 'async) c, 'k) meta * 'k -> t

let of_string ?off ?len x =
  let off, len =
    match (off, len) with
    | None, None -> (0, String.length x)
    | None, Some len -> (0, len)
    | Some off, None -> (off, String.length x - off)
    | Some off, Some len -> (off, len)
  in
  match commit ~off ~len string x with
  | Ok meta -> V (meta, x)
  | Error _ -> invalid_bounds off len (String.length x)

let of_bytes ?off ?len x =
  let off, len =
    match (off, len) with
    | None, None -> (0, Bytes.length x)
    | None, Some len -> (0, len)
    | Some off, None -> (off, Bytes.length x - off)
    | Some off, Some len -> (off, len)
  in
  match commit ~off ~len bytes x with
  | Ok meta -> V (meta, x)
  | Error _ -> invalid_bounds off len (Bytes.length x)

let of_bigstring ?off ?len x =
  let off, len =
    match (off, len) with
    | None, None -> (0, Bigstring.length x)
    | None, Some len -> (0, len)
    | Some off, None -> (off, Bigstring.length x - off)
    | Some off, Some len -> (off, len)
  in
  match commit ~off ~len bigstring x with
  | Ok meta -> V (meta, x)
  | Error _ -> invalid_bounds off len (Bigstring.length x)

let copy (V (meta, k)) =
  match Access.read (coerce meta) with
  | Some Refl.Refl ->
      let meta, k = copy meta k in
      V (meta, k)
  | None -> invalid_arg "No read access"

let sub (V (meta, k)) ~off ~len =
  match Access.read (coerce meta) with
  | Some Refl.Refl ->
      let meta, k = sub meta ~off ~len k in
      V (meta, k)
  | None -> invalid_arg "No read access"

let length (V (meta, k)) = length meta k

let unsafe_get (V (meta, k)) off =
  match Access.read (coerce meta) with
  | Some Refl.Refl -> unsafe_get meta k off
  | None -> invalid_arg "No read access"

let unsafe_set (V (meta, k)) off chr =
  match Access.write (coerce meta) with
  | Some Refl.Refl -> unsafe_set meta k off chr
  | None -> invalid_arg "No write access"

let get (V (meta, k)) off =
  match Access.read (coerce meta) with
  | Some Refl.Refl -> get meta k off
  | None -> invalid_arg "No read access"

let set (V (meta, k)) off chr =
  match Access.write (coerce meta) with
  | Some Refl.Refl -> set meta k off chr
  | None -> invalid_arg "No write access"

let pp ppf (V (meta, k)) =
  match Access.read (coerce meta) with
  | Some Refl.Refl -> pp meta ppf k
  | None -> invalid_arg "No read access"

let compare (V (meta0, k0)) (V (meta1, k1)) =
  match
    ( Access.read (coerce meta0)
    , Access.read (coerce meta1)
    , Access.equal (coerce meta0) (coerce meta1) )
  with
  | Some Refl.Refl, Some Refl.Refl, Some Refl.Refl ->
      compare ~a:meta0 ~b:meta1 k0 k1
  | Some Refl.Refl, Some Refl.Refl, None ->
      invalid_arg "Expect same kind of buffer"
  | _, _, _ -> invalid_arg "No read access"

let equal (V (meta0, k0)) (V (meta1, k1)) =
  match
    ( Access.read (coerce meta0)
    , Access.read (coerce meta1)
    , Access.equal (coerce meta0) (coerce meta1) )
  with
  | Some Refl.Refl, Some Refl.Refl, Some Refl.Refl ->
      equal ~a:meta0 ~b:meta1 k0 k1
  | Some Refl.Refl, Some Refl.Refl, None ->
      invalid_arg "Expect same kind of buffer"
  | _, _, _ -> invalid_arg "No read access"
