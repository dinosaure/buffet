open S

type rd = [`Read]
type wr = [`Write]
type async = [`Async]
type aligned = [`Aligned]
type capabilities = [rd | wr | async | aligned]

let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

let invalid_bounds off len real_len =
  invalid_arg "Invalid bounds on buffer (len: %d): off:%d, len:%d" real_len off
    len

exception Break

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"

module type X0 = sig
  type 'a t

  val length : _ t -> int
  external unsafe_get_int16 : [> rd] t -> int -> int = "%caml_string_get16u"
  external unsafe_get_int32 : [> rd] t -> int -> int32 = "%caml_string_get32u"
  external unsafe_get_int64 : [> rd] t -> int -> int64 = "%caml_string_get64u"
end

module GE (X : X0) = struct
  open X

  let unsafe_get_int16_le t off =
    if Sys.big_endian then swap16 (unsafe_get_int16 t off)
    else unsafe_get_int16 t off

  let unsafe_get_int16_be t off =
    if Sys.big_endian then unsafe_get_int16 t off
    else swap16 (unsafe_get_int16 t off)

  let get_int16_le t off =
    if off < 0 || off > length t - 2 then invalid_bounds off 2 (length t)
    else unsafe_get_int16_le t off

  let get_int16_be t off =
    if off < 0 || off > length t - 2 then invalid_bounds off 2 (length t)
    else unsafe_get_int16_be t off

  let unsafe_get_int32_le t off =
    if Sys.big_endian then swap32 (unsafe_get_int32 t off)
    else unsafe_get_int32 t off

  let unsafe_get_int32_be t off =
    if Sys.big_endian then unsafe_get_int32 t off
    else swap32 (unsafe_get_int32 t off)

  let get_int32_le t off =
    if off < 0 || off > length t - 4 then invalid_bounds off 4 (length t)
    else unsafe_get_int32_le t off

  let get_int32_be t off =
    if off < 0 || off > length t - 4 then invalid_bounds off 4 (length t)
    else unsafe_get_int32_be t off

  let unsafe_get_int64_le t off =
    if Sys.big_endian then swap64 (unsafe_get_int64 t off)
    else unsafe_get_int64 t off

  let unsafe_get_int64_be t off =
    if Sys.big_endian then unsafe_get_int64 t off
    else swap64 (unsafe_get_int64 t off)

  let get_int64_le t off =
    if off < 0 || off > length t - 8 then invalid_bounds off 8 (length t)
    else unsafe_get_int64_le t off

  let get_int64_be t off =
    if off < 0 || off > length t - 8 then invalid_bounds off 8 (length t)
    else unsafe_get_int64_be t off
end

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
end = struct
  type 'a t = bytes constraint 'a = [< capabilities]

  external length : _ t -> int = "%bytes_length"
  external unsafe_get : [> rd] t -> int -> char = "%bytes_unsafe_get"
  external unsafe_set : [> wr] t -> int -> char -> unit = "%bytes_unsafe_set"
  external create : int -> [rd | wr] t = "caml_create_bytes"

  external unsafe_fill :
    [> wr] t -> int -> int -> char -> unit
    = "caml_fill_bytes"
    [@@noalloc]

  external unsafe_blit :
    [> rd] t -> int -> [> wr] t -> int -> int -> unit
    = "caml_blit_bytes"
    [@@noalloc]

  let make len chr =
    let rs = create len in
    unsafe_fill rs 0 len chr ; rs

  let empty = create 0

  let unsafe_copy t ~off ~len =
    let rs = create len in
    unsafe_blit t off rs 0 len ; rs

  let copy t ~off ~len =
    if off < 0 || len < 0 || off > length t - len then
      invalid_bounds off len (length t) ;
    unsafe_copy t ~off ~len

  let unsafe_sub = unsafe_copy
  let sub = copy

  let get t off =
    if off < 0 || off > length t - 1 then invalid_bounds off 1 (length t) ;
    unsafe_get t off

  let set t off chr =
    if off < 0 || off > length t - 1 then invalid_bounds off 1 (length t) ;
    unsafe_set t off chr

  include GE (struct
    type 'a t = bytes

    let length x = length x

    external unsafe_get_int16 : [> rd] t -> int -> int = "%caml_string_get16u"

    external unsafe_get_int32 :
      [> rd] t -> int -> int32
      = "%caml_string_get32u"

    external unsafe_get_int64 :
      [> rd] t -> int -> int64
      = "%caml_string_get64u"
  end)

  external compare : [> rd] t -> [> rd] t -> int = "caml_bytes_compare"
    [@@noalloc]

  external equal : [> rd] t -> [> rd] t -> bool = "caml_bytes_equal"
    [@@noalloc]

  external unsafe_get_compare : bytes -> int -> int = "%bytes_unsafe_get"

  let unsafe_sub_compare ~a ~b ak bk =
    if a.len < b.len then -1
    else if a.len > b.len then 1
    else
      let rs = ref 0 in
      try
        for i = 0 to a.len - 1 do
          rs :=
            unsafe_get_compare ak (a.off + i)
            - unsafe_get_compare bk (b.off + i) ;
          if !rs <> 0 then raise Break
        done ;
        0
      with Break -> !rs

  let sub_compare ~a ~b ak bk =
    if
      a.off < 0
      || b.off < 0
      || a.len < 0
      || b.len < 0
      || a.off > length ak - a.len
      || b.off > length bk - b.len
    then
      invalid_arg
        "Invalid bounds: a:{ @[<hov>off = %d;@ len = %d;@] }, b:{ @[<hov>off \
         = %d; len = %d;@] }, real-length of a: %d, real-length of b: %d"
        a.off a.len b.off b.len (length ak) (length bk)
    else unsafe_sub_compare ~a ~b ak bk

  let unsafe_sub_equal ~a ~b ak bk = unsafe_sub_compare ~a ~b ak bk = 0
  let sub_equal ~a ~b ak bk = sub_compare ~a ~b ak bk = 0
  let pp _ppf _x = ()
  let unsafe_sub_pp ~off:_ ~len:_ _ppf _x = ()
  let sub_pp ~off:_ ~len:_ _ppf _x = ()

  external unsafe_set_int16 :
    bytes -> int -> int -> unit
    = "%caml_string_set16u"

  external unsafe_set_int32 :
    bytes -> int -> int32 -> unit
    = "%caml_string_set32u"

  external unsafe_set_int64 :
    bytes -> int -> int64 -> unit
    = "%caml_string_set64u"

  let unsafe_set_int16_le t off v =
    if Sys.big_endian then unsafe_set_int16 t off (swap16 v)
    else unsafe_set_int16 t off v

  let unsafe_set_int16_be t off v =
    if Sys.big_endian then unsafe_set_int16 t off v
    else unsafe_set_int16 t off v

  let set_int16_le t off v =
    if off < 0 || off > length t - 2 then invalid_bounds off 2 (length t)
    else unsafe_set_int16_le t off v

  let set_int16_be t off v =
    if off < 0 || off > length t - 2 then invalid_bounds off 2 (length t)
    else unsafe_set_int16_be t off v

  let unsafe_set_int32_le t off v =
    if Sys.big_endian then unsafe_set_int32 t off (swap32 v)
    else unsafe_set_int32 t off v

  let unsafe_set_int32_be t off v =
    if Sys.big_endian then unsafe_set_int32 t off v
    else unsafe_set_int32 t off (swap32 v)

  let set_int32_le t off v =
    if off < 0 || off > length t - 4 then invalid_bounds off 4 (length t)
    else unsafe_set_int32_le t off v

  let set_int32_be t off v =
    if off < 0 || off > length t - 4 then invalid_bounds off 4 (length t)
    else unsafe_set_int32_be t off v

  let unsafe_set_int64_le t off v =
    if Sys.big_endian then unsafe_set_int64 t off (swap64 v)
    else unsafe_set_int64 t off v

  let unsafe_set_int64_be t off v =
    if Sys.big_endian then unsafe_set_int64 t off v
    else unsafe_set_int64 t off (swap64 v)

  let set_int64_le t off v =
    if off < 0 || off > length t - 8 then invalid_bounds off 8 (length t)
    else unsafe_set_int64_le t off v

  let set_int64_be t off v =
    if off < 0 || off > length t - 8 then invalid_bounds off 8 (length t)
    else unsafe_set_int64_le t off v

  let unsafe_blit src ~src_off dst ~dst_off ~len =
    unsafe_blit src src_off dst dst_off len

  let invalid_blit src_off dst_off len src_real_len dst_real_len =
    invalid_arg
      "Invalid blit (src, off:%d len:%d real-len:%d) (dst, off:%d len:%d \
       real-len:%d)"
      src_off len src_real_len dst_off len dst_real_len

  let blit src ~src_off dst ~dst_off ~len =
    if
      len < 0
      || src_off < 0
      || dst_off < 0
      || src_off > length src - len
      || dst_off > length dst - len
    then invalid_blit src_off dst_off len (length src) (length dst) ;
    unsafe_blit src ~src_off dst ~dst_off ~len
end
