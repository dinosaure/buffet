open Buffet6

module H = struct
  module Bytes = Higher.Make (Bytes)
  module String = Higher.Make (String)
  module Bigstring = Higher.Make (Bigstring)
end

type 'k tag =
  | Bytes : H.Bytes.t tag
  | String : H.String.t tag
  | Bigstring : H.Bigstring.t tag

type bytes = H.Bytes.t
type string = H.String.t
type bigstring = H.Bigstring.t

(* / *)

type ('a, 'k) witness = 'k tag
type ('a, 'k) t = ('a, 'k) Higher.app

let bytes : ([rd | wr], H.Bytes.t) witness = Bytes
let string : (rd, H.String.t) witness = String
let bigstring : ([rd | wr | async], H.Bigstring.t) witness = Bigstring

type 'a value =
  | Bytes of 'a Bytes.t
  | String of 'a String.t
  | Bigstring of 'a Bigstring.t

module Injection = struct
  let bytes : 'a Bytes.t -> ('a, bytes) t = H.Bytes.inj
  let string : 'a String.t -> ('a, string) t = H.String.inj
  let bigstring : 'a Bigstring.t -> ('a, bigstring) t = H.Bigstring.inj
end

let get : type k. ([> rd], k) witness -> ([> rd], k) t -> int -> char =
 fun witness buf off ->
  match witness with
  | String -> (H.String.prj buf).[off]
  | Bytes -> Bytes.get (H.Bytes.prj buf) off
  | Bigstring -> Bigstring.get (H.Bigstring.prj buf) off

let unsafe_get : type k. ([> rd], k) witness -> ([> rd], k) t -> int -> char =
 fun witness buf off ->
  match witness with
  | String -> String.unsafe_get (H.String.prj buf) off
  | Bytes -> Bytes.unsafe_get (H.Bytes.prj buf) off
  | Bigstring -> Bigstring.unsafe_get (H.Bigstring.prj buf) off

let set : type k. ([> wr], k) witness -> ([> wr], k) t -> int -> char -> unit =
 fun witness buf off chr ->
  match witness with
  | Bytes -> Bytes.set (H.Bytes.prj buf) off chr
  | Bigstring -> Bigstring.set (H.Bigstring.prj buf) off chr
  | String -> invalid_arg "No write access"

let unsafe_set : type k.
    ([> wr], k) witness -> ([> wr], k) t -> int -> char -> unit =
 fun witness buf off chr ->
  match witness with
  | Bytes -> Bytes.unsafe_set (H.Bytes.prj buf) off chr
  | Bigstring -> Bigstring.unsafe_set (H.Bigstring.prj buf) off chr
  | String -> invalid_arg "No write access"
