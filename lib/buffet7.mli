open Buffet6

type ('a, 'k) witness
type ('a, 'k) t

type 'a value =
  | Bytes of 'a Bytes.t
  | String of 'a String.t
  | Bigstring of 'a Bigstring.t

type bytes
type string
type bigstring

val bytes : ([rd | wr], bytes) witness
val string : (rd, string) witness
val bigstring : ([rd | wr | async], bigstring) witness

module Injection : sig
  val bytes : 'a Bytes.t -> ('a, bytes) t
  val string : 'a String.t -> ('a, string) t
  val bigstring : 'a Bigstring.t -> ('a, bigstring) t
end

val create : ('a, 'k) witness -> int -> ([< rd | wr], 'k) t
val get : ([> rd], 'k) witness -> ([> rd], 'k) t -> int -> char
val unsafe_get : ([> rd], 'k) witness -> ([> rd], 'k) t -> int -> char
val set : ([> wr], 'k) witness -> ([> wr], 'k) t -> int -> char -> unit
val unsafe_set : ([> wr], 'k) witness -> ([> wr], 'k) t -> int -> char -> unit
