open S

type 'a rd = < rd: unit ; .. > as 'a
type 'a wr = < wr: unit ; .. > as 'a
type 'a async = < async: unit ; .. > as 'a
type 'a aligned = < aligned: unit ; .. > as 'a

let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

let invalid_bounds off len real_len =
  invalid_arg "Invalid bounds on buffer (len: %d): off:%d, len:%d" real_len off
    len

exception Break

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"

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

module type X0 = sig
  type 'a t

  val length : _ t -> int
  external unsafe_get_int16 : 'a rd t -> int -> int = "%caml_string_get16u"
  external unsafe_get_int32 : 'a rd t -> int -> int32 = "%caml_string_get32u"
  external unsafe_get_int64 : 'a rd t -> int -> int64 = "%caml_string_get64u"
end

module GE (X : X0) = struct
  open X

  let unsafe_get_int16_le =
    if Sys.big_endian then fun t off -> swap16 (unsafe_get_int16 t off)
    else fun t off -> unsafe_get_int16 t off

  let unsafe_get_int16_be =
    if Sys.big_endian then fun t off -> unsafe_get_int16 t off
    else fun t off -> swap16 (unsafe_get_int16 t off)

  let get_int16_le t off =
    if off < 0 || off > length t - 2 then invalid_bounds off 2 (length t)
    else unsafe_get_int16_le t off

  let get_int16_be t off =
    if off < 0 || off > length t - 2 then invalid_bounds off 2 (length t)
    else unsafe_get_int16_be t off

  let unsafe_get_int32_le =
    if Sys.big_endian then fun t off -> swap32 (unsafe_get_int32 t off)
    else fun t off -> unsafe_get_int32 t off

  let unsafe_get_int32_be =
    if Sys.big_endian then fun t off -> unsafe_get_int32 t off
    else fun t off -> swap32 (unsafe_get_int32 t off)

  let get_int32_le t off =
    if off < 0 || off > length t - 4 then invalid_bounds off 4 (length t)
    else unsafe_get_int32_le t off

  let get_int32_be t off =
    if off < 0 || off > length t - 4 then invalid_bounds off 4 (length t)
    else unsafe_get_int32_be t off

  let unsafe_get_int64_le =
    if Sys.big_endian then fun t off -> swap64 (unsafe_get_int64 t off)
    else fun t off -> unsafe_get_int64 t off

  let unsafe_get_int64_be =
    if Sys.big_endian then fun t off -> unsafe_get_int64 t off
    else fun t off -> swap64 (unsafe_get_int64 t off)

  let get_int64_le t off =
    if off < 0 || off > length t - 8 then invalid_bounds off 8 (length t)
    else unsafe_get_int64_le t off

  let get_int64_be t off =
    if off < 0 || off > length t - 8 then invalid_bounds off 8 (length t)
    else unsafe_get_int64_be t off
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
end = struct
  type 'a t = bytes

  external length : _ t -> int = "%bytes_length"
  external unsafe_get : 'a rd t -> int -> char = "%bytes_unsafe_get"
  external unsafe_set : 'a wr t -> int -> char -> unit = "%bytes_unsafe_set"
  external create : int -> < rd: unit ; wr: unit > t = "caml_create_bytes"
  external ro : 'a rd t -> < rd: unit > t = "%identity"
  external wo : 'a wr t -> < wr: unit > t = "%identity"

  external unsafe_fill :
    'a wr t -> int -> int -> char -> unit
    = "caml_fill_bytes"
    [@@noalloc]

  external unsafe_blit :
    'a rd t -> int -> 'a wr t -> int -> int -> unit
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

    external unsafe_get_int16 : 'a rd t -> int -> int = "%caml_string_get16u"
    external unsafe_get_int32 : 'a rd t -> int -> int32 = "%caml_string_get32u"
    external unsafe_get_int64 : 'a rd t -> int -> int64 = "%caml_string_get64u"
  end)

  external compare : 'a rd t -> 'a rd t -> int = "caml_bytes_compare"
    [@@noalloc]

  external equal : 'a rd t -> 'a rd t -> bool = "caml_bytes_equal" [@@noalloc]
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

  let unsafe_set_int16_le =
    if Sys.big_endian then fun t off v -> unsafe_set_int16 t off (swap16 v)
    else fun t off v -> unsafe_set_int16 t off v

  let unsafe_set_int16_be =
    if Sys.big_endian then fun t off v -> unsafe_set_int16 t off v
    else fun t off v -> unsafe_set_int16 t off v

  let set_int16_le t off v =
    if off < 0 || off > length t - 2 then invalid_bounds off 2 (length t)
    else unsafe_set_int16_le t off v

  let set_int16_be t off v =
    if off < 0 || off > length t - 2 then invalid_bounds off 2 (length t)
    else unsafe_set_int16_be t off v

  let unsafe_set_int32_le =
    if Sys.big_endian then fun t off v -> unsafe_set_int32 t off (swap32 v)
    else fun t off v -> unsafe_set_int32 t off v

  let unsafe_set_int32_be =
    if Sys.big_endian then fun t off v -> unsafe_set_int32 t off v
    else fun t off v -> unsafe_set_int32 t off (swap32 v)

  let set_int32_le t off v =
    if off < 0 || off > length t - 4 then invalid_bounds off 4 (length t)
    else unsafe_set_int32_le t off v

  let set_int32_be t off v =
    if off < 0 || off > length t - 4 then invalid_bounds off 4 (length t)
    else unsafe_set_int32_be t off v

  let unsafe_set_int64_le =
    if Sys.big_endian then fun t off v -> unsafe_set_int64 t off (swap64 v)
    else fun t off v -> unsafe_set_int64 t off v

  let unsafe_set_int64_be =
    if Sys.big_endian then fun t off v -> unsafe_set_int64 t off v
    else fun t off v -> unsafe_set_int64 t off (swap64 v)

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

module String : sig
  type 'a t = string

  val make : int -> char -> < rd: unit > t
  val empty : < rd: unit > t

  include S0 with type 'a t := 'a t
end = struct
  type 'a t = string

  external length : _ t -> int = "%string_length"
  external unsafe_get : 'a rd t -> int -> char = "%string_unsafe_get"

  external unsafe_bytes_to_string :
    'a rd Bytes.t -> < rd: unit > t
    = "%bytes_to_string"

  external unsafe_string_to_bytes :
    'a rd t -> 'a rd Bytes.t
    = "%bytes_of_string"

  let get buf off =
    if off < 0 || off > length buf - 1 then invalid_bounds off 1 (length buf)
    else unsafe_get buf off

  let make len chr = unsafe_bytes_to_string (Bytes.make len chr)
  let empty = unsafe_bytes_to_string (Bytes.create 0)

  let unsafe_copy t ~off ~len =
    unsafe_bytes_to_string
      (Bytes.unsafe_copy (unsafe_string_to_bytes t) ~off ~len)

  let copy t ~off ~len =
    unsafe_bytes_to_string (Bytes.copy (unsafe_string_to_bytes t) ~off ~len)

  let unsafe_sub t ~off ~len =
    unsafe_bytes_to_string
      (Bytes.unsafe_sub (unsafe_string_to_bytes t) ~off ~len)

  let sub t ~off ~len =
    unsafe_bytes_to_string (Bytes.sub (unsafe_string_to_bytes t) ~off ~len)

  include GE (struct
    type 'a t = string

    let length x = length x

    external unsafe_get_int16 : 'a rd t -> int -> int = "%caml_string_get16u"
    external unsafe_get_int32 : 'a rd t -> int -> int32 = "%caml_string_get32u"
    external unsafe_get_int64 : 'a rd t -> int -> int64 = "%caml_string_get64u"
  end)

  external compare : string -> string -> int = "caml_string_compare"
    [@@noalloc]

  external unsafe_get_compare : string -> int -> int = "%string_unsafe_get"

  let unsafe_sub_compare ~a ~b ak bk =
    if a.len < b.len then -1
    else if a.len > b.len then 1
    else
      let rs = ref 0 in
      try
        for i = 0 to a.len do
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

  external equal : string -> string -> bool = "caml_string_equal" [@@noalloc]

  let unsafe_sub_equal ~a ~b ak bk = unsafe_sub_compare ~a ~b ak bk = 0
  let sub_equal ~a ~b ak bk = sub_compare ~a ~b ak bk = 0
  let pp _ppf _x = ()
  let unsafe_sub_pp ~off:_ ~len:_ _ppf _x = ()
  let sub_pp ~off:_ ~len:_ _ppf _x = ()
end

module Bigstring : sig
  type 'a t = private Bigstringaf.t

  val create : int -> < rd: unit ; wr: unit ; async: unit > t
  val make : int -> char -> < rd: unit ; wr: unit ; async: unit > t
  val empty : < rd: unit ; wr: unit ; async: unit > t
  val ro : 'a rd t -> < rd: unit > t
  val wo : 'a wr t -> < wr: unit > t

  include S0 with type 'a t := 'a t
  include S1 with type 'a t := 'a t
end = struct
  include (
    Bigstringaf : module type of Bigstringaf with type t := Bigstringaf.t )

  (* XXX(dinosaure): (;_;) *)

  type 'a t = Bigstringaf.t

  external ro : 'a rd t -> < rd: unit > t = "%identity"
  external wo : 'a wr t -> < wr: unit > t = "%identity"
  external unsafe_fill : 'a wr t -> char -> unit = "caml_ba_fill"

  let make len chr =
    let rs = create len in
    unsafe_fill rs chr ; rs

  let unsafe_sub t ~off ~len = sub t ~off ~len
  let unsafe_copy t ~off ~len = sub t ~off ~len
  let pp _ppf _x = ()

  external unsafe_get_compare : 'a rd t -> int -> int = "%caml_ba_unsafe_ref_1"

  (* XXX(dinosaure): not sure! *)

  let compare a b =
    if length a < length b then 1
    else if length a > length b then -1
    else
      let ln = length a in
      let rs = ref 0 in
      try
        for i = 0 to ln - 1 do
          rs := unsafe_get_compare a i - unsafe_get_compare b i ;
          if !rs <> 0 then raise Break
        done ;
        0
      with Break -> !rs

  let unsafe_sub_compare ~a ~b ak bk =
    compare
      (unsafe_sub ~off:a.off ~len:a.len ak)
      (unsafe_sub ~off:b.off ~len:b.len bk)

  let sub_compare ~a ~b ak bk =
    compare (sub ~off:a.off ~len:a.len ak) (sub ~off:b.off ~len:b.len bk)

  let equal a b = compare a b = 0
  let unsafe_sub_equal ~a ~b ak bk = unsafe_sub_compare ~a ~b ak bk = 0
  let sub_equal ~a ~b ak bk = sub_compare ~a ~b ak bk = 0
  let unsafe_sub_pp ~off:_ ~len:_ _ppf _x = ()
  let sub_pp ~off:_ ~len:_ _ppf _x = ()
end
