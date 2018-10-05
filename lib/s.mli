type ('k, 'v) get = 'k -> int -> 'v

type ('k, 'v) set = 'k -> int -> 'v -> unit

type ('k, 'v) fill = 'k -> 'v -> unit

type ('k0, 'k1) blit =
  'k0 -> src_off:int -> 'k1 -> dst_off:int -> len:int -> unit

type 'k length = 'k -> int

type ('k0, 'k1) sub = 'k0 -> off:int -> len:int -> 'k1

type ('k0, 'k1) copy = 'k0 -> off:int -> len:int -> 'k1

type 'k compare = 'k -> 'k -> int

type 'k equal = 'k -> 'k -> bool

type 'k fmt = Format.formatter -> 'k -> unit

type slice = {off: int; len: int}

module type S0 = sig
  type t

  val make : int -> char -> t

  val empty : t

  val unsafe_copy : (t, t) copy

  val copy : (t, t) copy

  val unsafe_sub : (t, t) sub

  val sub : (t, t) sub

  val length : t -> int

  val unsafe_get : t -> int -> char

  val get : t -> int -> char

  val unsafe_get_int16_le : (t, int) get

  val get_int16_le : (t, int) get

  val unsafe_get_int16_be : (t, int) get

  val get_int16_be : (t, int) get

  val unsafe_get_int32_le : (t, int32) get

  val get_int32_le : (t, int32) get

  val unsafe_get_int32_be : (t, int32) get

  val get_int32_be : (t, int32) get

  val unsafe_get_int64_le : (t, int64) get

  val get_int64_le : (t, int64) get

  val unsafe_get_int64_be : (t, int64) get

  val get_int64_be : (t, int64) get

  val compare : t compare

  val unsafe_sub_compare : a:slice -> b:slice -> t compare

  val sub_compare : a:slice -> b:slice -> t compare

  val equal : t equal

  val unsafe_sub_equal : a:slice -> b:slice -> t equal

  val sub_equal : a:slice -> b:slice -> t equal

  val pp : t fmt

  val unsafe_sub_pp : off:int -> len:int -> t fmt

  val sub_pp : off:int -> len:int -> t fmt
end

module type S1 = sig
  type t

  val create : int -> t

  val unsafe_set : (t, char) set

  val set : (t, char) set

  val unsafe_set_int16_le : (t, int) set

  val set_int16_le : (t, int) set

  val unsafe_set_int16_be : (t, int) set

  val set_int16_be : (t, int) set

  val unsafe_set_int32_le : (t, int32) set

  val set_int32_le : (t, int32) set

  val unsafe_set_int32_be : (t, int32) set

  val set_int32_be : (t, int32) set

  val unsafe_set_int64_le : (t, int64) set

  val set_int64_le : (t, int64) set

  val unsafe_set_int64_be : (t, int64) set

  val set_int64_be : (t, int64) set

  val unsafe_blit : (t, t) blit

  val blit : (t, t) blit
end
