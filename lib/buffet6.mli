open S

type rd = [`Read]
type wr = [`Write]
type async = [`Async]
type aligned = [`Aligned]
type capabilities = [rd | wr | async | aligned]

module Bytes : sig
  type 'a t = private bytes constraint 'a = [< capabilities]

  val length : _ t -> int
  val unsafe_get : ([> rd] t, char) get
  val unsafe_set : ([> wr] t, char) set
  val create : int -> [rd | wr] t
  val unsafe_blit : ([> rd] t, [> wr] t) blit
  val blit : ([> rd] t, [> wr] t) blit
  val make : int -> char -> [rd | wr] t
  val empty : [rd | wr] t
  val unsafe_copy : ([> rd] t, [rd | wr] t) copy
  val copy : ([> rd] t, [rd | wr] t) copy
  val unsafe_sub : ([> rd] t, [rd | wr] t) sub
  val sub : ([> rd] t, [rd | wr] t) sub
  val get : ([> rd] t, char) get
  val set : ([> wr] t, char) set
  val compare : [> rd] t compare
  val equal : [> rd] t equal
  val sub_compare : a:slice -> b:slice -> [> rd] t compare
  val sub_equal : a:slice -> b:slice -> [> rd] t equal
  val unsafe_sub_compare : a:slice -> b:slice -> [> rd] t compare
  val unsafe_sub_equal : a:slice -> b:slice -> [> rd] t equal
  val pp : [> rd] t fmt
  val unsafe_sub_pp : off:int -> len:int -> [> rd] t fmt
  val sub_pp : off:int -> len:int -> [> rd] t fmt
  val unsafe_get_int16_le : ([> rd] t, int) get
  val get_int16_le : ([> rd] t, int) get
  val unsafe_get_int16_be : ([> rd] t, int) get
  val get_int16_be : ([> rd] t, int) get
  val unsafe_get_int32_le : ([> rd] t, int32) get
  val get_int32_le : ([> rd] t, int32) get
  val unsafe_get_int32_be : ([> rd] t, int32) get
  val get_int32_be : ([> rd] t, int32) get
  val unsafe_get_int64_le : ([> rd] t, int64) get
  val get_int64_le : ([> rd] t, int64) get
  val unsafe_get_int64_be : ([> rd] t, int64) get
  val get_int64_be : ([> rd] t, int64) get
  val unsafe_set_int16_le : ([> wr] t, int) set
  val set_int16_le : ([> wr] t, int) set
  val unsafe_set_int16_be : ([> wr] t, int) set
  val set_int16_be : ([> wr] t, int) set
  val unsafe_set_int32_le : ([> wr] t, int32) set
  val set_int32_le : ([> wr] t, int32) set
  val unsafe_set_int32_be : ([> wr] t, int32) set
  val set_int32_be : ([> wr] t, int32) set
  val unsafe_set_int64_le : ([> wr] t, int64) set
  val set_int64_le : ([> wr] t, int64) set
  val unsafe_set_int64_be : ([> wr] t, int64) set
  val set_int64_be : ([> wr] t, int64) set
end
