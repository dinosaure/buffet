open S
open Buffet0

type bytes = Bytes.t
type string = String.t
type bigstring = Bigstring.t
type 'k tag

val bytes : bytes tag
val string : string tag
val bigstring : bigstring tag

type value = Bytes of bytes | String of string | Bigstring of bigstring

val make : 'k tag -> int -> char -> 'k
val empty : 'k tag -> 'k
val unsafe_copy : 'k tag -> ('k, 'k) copy
val copy : 'k tag -> ('k, 'k) copy
val unsafe_sub : 'k tag -> ('k, 'k) sub
val sub : 'k tag -> ('k, 'k) sub
val create : 'k tag -> int -> 'k
val length : 'k tag -> 'k length
val unsafe_get : 'k tag -> ('k, char) get
val unsafe_set : 'k tag -> ('k, char) set
val get : 'k tag -> ('k, char) get
val set : 'k tag -> ('k, char) set
val pp : 'k tag -> 'k fmt
val unsafe_sub_pp : 'k tag -> off:int -> len:int -> 'k fmt
val sub_pp : 'k tag -> off:int -> len:int -> 'k fmt
val compare : 'k tag -> 'k compare
val unsafe_sub_compare : 'k tag -> a:slice -> b:slice -> 'k compare
val sub_compare : 'k tag -> a:slice -> b:slice -> 'k compare
val equal : 'k tag -> 'k equal
val unsafe_sub_equal : 'k tag -> a:slice -> b:slice -> 'k equal
val sub_equal : 'k tag -> a:slice -> b:slice -> 'k equal
val to_value : 'k tag -> 'k -> value

type v = V : 'k tag * 'k -> v

val of_value : value -> v

module Tag : sig
  type 'k t = 'k tag

  val equal : 'a t -> 'b t -> ('a, 'b) Refl.t option
end

val coerce : value -> 'k tag -> ('k tag * 'k) option
