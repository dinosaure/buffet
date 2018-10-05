open S
open Buffet1

type ('rd, 'wr, 'async) c = < rd: 'rd ; wr: 'wr ; async: 'async >
type t = [`True]
type f = [`False]
type 'async ro = (t, f, 'async) c
type 'async wo = (f, t, 'async) c
type 'async rdwr = (t, t, 'async) c
type ('a, 'k) access

val bytes : (f rdwr, bytes) access
val string : (f ro, string) access
val bigstring : (t rdwr, bigstring) access
val make : ('a, 'k) access -> int -> char -> 'k
val empty : ('a, 'k) access -> 'k
val unsafe_copy : ((t, 'wr, 'async) c, 'k) access -> ('k, 'k) copy
val copy : ((t, 'wr, 'async) c, 'k) access -> ('k, 'k) copy
val unsafe_sub : ((t, 'wr, 'async) c, 'k) access -> ('k, 'k) sub
val sub : ((t, 'wr, 'async) c, 'k) access -> ('k, 'k) sub
val create : (('rd, t, 'async) c, 'k) access -> int -> 'k
val length : ('a, 'k) access -> 'k -> int
val unsafe_get : ((t, 'wr, 'async) c, 'k) access -> ('k, char) get
val unsafe_set : (('rd, t, 'async) c, 'k) access -> ('k, char) set
val get : ((t, 'wr, 'async) c, 'k) access -> ('k, char) get
val set : (('rd, t, 'async) c, 'k) access -> ('k, char) set
val pp : ((t, 'wr, 'async) c, 'k) access -> 'k fmt

val unsafe_sub_pp :
  ((t, 'wr, 'async) c, 'k) access -> off:int -> len:int -> 'k fmt

val sub_pp : ((t, 'wr, 'async) c, 'k) access -> off:int -> len:int -> 'k fmt

val unsafe_sub_compare :
  ((t, 'wr, 'async) c, 'k) access -> a:slice -> b:slice -> 'k compare

val sub_compare :
  ((t, 'wr, 'async) c, 'k) access -> a:slice -> b:slice -> 'k compare

val compare : ((t, 'wr, 'async) c, 'k) access -> 'k compare

val unsafe_sub_equal :
  ((t, 'wr, 'async) c, 'k) access -> a:slice -> b:slice -> 'k equal

val sub_equal :
  ((t, 'wr, 'async) c, 'k) access -> a:slice -> b:slice -> 'k equal

val equal : ((t, 'wr, 'async) c, 'k) access -> 'k equal

module Access : sig
  type ('a, 'k) t = ('a, 'k) access

  val equal : ('a0, 'k0) t -> ('a1, 'k1) t -> ('k0, 'k1) Refl.t option
  val read : (('rd, 'wr, 'async) c, 'k) t -> ('rd, [`True]) Refl.t option
  val write : (('rd, 'wr, 'async) c, 'k) t -> ('wr, [`True]) Refl.t option
  val async : (('rd, 'wr, 'async) c, 'k) t -> ('async, [`True]) Refl.t option
end

val coerce : ('a, 'k) access -> 'k tag
