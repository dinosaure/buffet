open S
open Buffet1

module Bool : sig
  type t = True
  type f = False
end

type ('rd, 'wr, 'async) c = < rd: 'rd ; wr: 'wr ; async: 'async >
type 'async ro = (Bool.t, Bool.f, 'async) c
type 'async wo = (Bool.f, Bool.t, 'async) c
type 'async rdwr = (Bool.t, Bool.t, 'async) c
type ('a, 'k) access

val bytes : (Bool.f rdwr, bytes) access
val string : (Bool.f ro, string) access
val bigstring : (Bool.t rdwr, bigstring) access

(* / *)

val make : ('a, 'k) access -> int -> char -> 'k
val empty : ('a, 'k) access -> 'k
val unsafe_copy : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, 'k) copy
val copy : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, 'k) copy
val unsafe_sub : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, 'k) sub
val sub : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, 'k) sub
val create : (('rd, Bool.t, 'async) c, 'k) access -> int -> 'k
val length : ('a, 'k) access -> 'k -> int
val unsafe_get : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, char) get
val unsafe_set : (('rd, Bool.t, 'async) c, 'k) access -> ('k, char) set
val get : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, char) get
val set : (('rd, Bool.t, 'async) c, 'k) access -> ('k, char) set
val pp : ((Bool.t, 'wr, 'async) c, 'k) access -> 'k fmt

val unsafe_sub_pp :
  ((Bool.t, 'wr, 'async) c, 'k) access -> off:int -> len:int -> 'k fmt

val sub_pp :
  ((Bool.t, 'wr, 'async) c, 'k) access -> off:int -> len:int -> 'k fmt

val unsafe_sub_compare :
  ((Bool.t, 'wr, 'async) c, 'k) access -> a:slice -> b:slice -> 'k compare

val sub_compare :
  ((Bool.t, 'wr, 'async) c, 'k) access -> a:slice -> b:slice -> 'k compare

val compare : ((Bool.t, 'wr, 'async) c, 'k) access -> 'k compare

val unsafe_sub_equal :
  ((Bool.t, 'wr, 'async) c, 'k) access -> a:slice -> b:slice -> 'k equal

val sub_equal :
  ((Bool.t, 'wr, 'async) c, 'k) access -> a:slice -> b:slice -> 'k equal

val equal : ((Bool.t, 'wr, 'async) c, 'k) access -> 'k equal

module Access : sig
  type ('a, 'k) t = ('a, 'k) access

  val equal : ('a0, 'k0) t -> ('a1, 'k1) t -> ('k0, 'k1) Refl.t option
  val read : (('rd, 'wr, 'async) c, 'k) t -> ('rd, Bool.t) Refl.t option
  val write : (('rd, 'wr, 'async) c, 'k) t -> ('wr, Bool.t) Refl.t option
  val async : (('rd, 'wr, 'async) c, 'k) t -> ('async, Bool.t) Refl.t option
end

val coerce : ('a, 'k) access -> 'k tag
