open S

type 'a rd = < rd: unit ; .. > as 'a
type 'a wr = < wr: unit ; .. > as 'a
type 'a async = < async: unit ; .. > as 'a
type 'a aligned = < aligned: unit ; .. > as 'a

(* / *)

module type S0 = sig
  type 'a t

  val unsafe_copy : ('a rd t, 'a rd t) copy
  val copy : ('a rd t, 'a rd t) copy
  val unsafe_sub : ('a rd t, 'a rd t) sub
  val sub : ('a rd t, 'a rd t) sub
  val length : _ t -> int
  val unsafe_get : 'a rd t -> int -> char
  val get : 'a rd t -> int -> char
  val unsafe_get_int16_le : ('a rd t, int) get
  val get_int16_le : ('a rd t, int) get
  val unsafe_get_int16_be : ('a rd t, int) get
  val get_int16_be : ('a rd t, int) get
  val unsafe_get_int32_le : ('a rd t, int32) get
  val get_int32_le : ('a rd t, int32) get
  val unsafe_get_int32_be : ('a rd t, int32) get
  val get_int32_be : ('a rd t, int32) get
  val unsafe_get_int64_le : ('a rd t, int64) get
  val get_int64_le : ('a rd t, int64) get
  val unsafe_get_int64_be : ('a rd t, int64) get
  val get_int64_be : ('a rd t, int64) get
  val compare : 'a rd t compare
  val unsafe_sub_compare : a:slice -> b:slice -> 'a rd t compare
  val sub_compare : a:slice -> b:slice -> 'a rd t compare
  val equal : 'a rd t equal
  val unsafe_sub_equal : a:slice -> b:slice -> 'a rd t equal
  val sub_equal : a:slice -> b:slice -> 'a rd t equal
  val pp : 'a rd t fmt
  val unsafe_sub_pp : off:int -> len:int -> 'a rd t fmt
  val sub_pp : off:int -> len:int -> 'a rd t fmt
end

module type S1 = sig
  type 'a t

  val unsafe_set : ('a wr t, char) set
  val set : ('a wr t, char) set
  val unsafe_set_int16_le : ('a wr t, int) set
  val set_int16_le : ('a wr t, int) set
  val unsafe_set_int16_be : ('a wr t, int) set
  val set_int16_be : ('a wr t, int) set
  val unsafe_set_int32_le : ('a wr t, int32) set
  val set_int32_le : ('a wr t, int32) set
  val unsafe_set_int32_be : ('a wr t, int32) set
  val set_int32_be : ('a wr t, int32) set
  val unsafe_set_int64_le : ('a wr t, int64) set
  val set_int64_le : ('a wr t, int64) set
  val unsafe_set_int64_be : ('a wr t, int64) set
  val set_int64_be : ('a wr t, int64) set
  val unsafe_blit : ('a rd t, 'a wr t) blit
  val blit : ('a rd t, 'a wr t) blit
end

module Bytes : sig
  type 'a t = private bytes

  val create : int -> < rd: unit ; wr: unit > t
  val make : int -> char -> < rd: unit ; wr: unit > t
  val empty : < rd: unit ; wr: unit > t
  val ro : 'a rd t -> < rd: unit > t
  val wo : 'a wr t -> < wr: unit > t

  include S0 with type 'a t := 'a t
  include S1 with type 'a t := 'a t
end

module String : sig
  type 'a t = private string

  val make : int -> char -> < rd: unit > t
  val empty : < rd: unit > t

  include S0 with type 'a t := 'a t
end

module Bigstring : sig
  type 'a t =
    private
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  val create : int -> < rd: unit ; wr: unit ; async: unit > t
  val make : int -> char -> < rd: unit ; wr: unit ; async: unit > t
  val empty : < rd: unit ; wr: unit ; async: unit > t
  val ro : 'a rd t -> < rd: unit > t
  val wo : 'a wr t -> < wr: unit > t

  include S0 with type 'a t := 'a t
  include S1 with type 'a t := 'a t
end
