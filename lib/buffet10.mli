open S
open Integer
open Buffet1

type 'a rd = < rd: unit ; .. > as 'a
type 'a wr = < wr: unit ; .. > as 'a
type 'a async = < async: unit ; .. > as 'a
type 'a aligned = < aligned: unit ; .. > as 'a

(* / *)

type rd_only = < rd: unit >
type wr_only = < wr: unit >

(* / *)

type ('a, 'k) t

val bytes : (< rd: unit ; wr: unit >, bytes) t
val string : (< rd: unit >, string) t
val bigstring : (< rd: unit ; wr: unit ; async: unit >, bigstring) t

(* / *)

val ro : ('a rd, 'k) t -> (rd_only, 'k) t
val wo : ('a wr, 'k) t -> (wr_only, 'k) t

(* / *)

val create : ('a wr, 'k) t -> int -> 'k
val make : ('a rd, 'k) t -> int -> char -> 'k
val empty : ('a rd, 'k) t -> 'k
val get : ('a rd, 'k) t -> ('k, char) get
val set : ('a wr, 'k) t -> ('k, char) set
val copy : ('a rd, 'k) t -> ('k, 'k) copy
val sub : ('a rd, 'k) t -> ('k, 'k) sub
val length : ('a, 'k) t -> 'k length
val pp : ('a rd, 'k) t -> 'k fmt
val sub_pp : ('a rd, 'k) t -> off:int -> len:int -> 'k fmt
val compare : ('a rd, 'k) t -> 'k compare
val sub_compare : ('a rd, 'k) t -> a:slice -> b:slice -> 'k compare
val equal : ('a rd, 'k) t -> 'k equal
val sub_equal : ('a rd, 'k) t -> a:slice -> b:slice -> 'k equal
val get_uint16_le : ('a rd, 'k) t -> ('k, [le | unsigned] int16) get
val get_uint16_be : ('a rd, 'k) t -> ('k, [be | unsigned] int16) get
val get_int16_le : ('a rd, 'k) t -> ('k, le int16) get
val get_int16_be : ('a rd, 'k) t -> ('k, be int16) get
val get_int32_le : ('a rd, 'k) t -> ('k, le int32) get
val get_int32_be : ('a rd, 'k) t -> ('k, be int32) get
val get_int64_le : ('a rd, 'k) t -> ('k, le int64) get
val get_int64_be : ('a rd, 'k) t -> ('k, be int64) get

(* / *)

val unsafe_get : ('a rd, 'k) t -> 'k -> int -> char
val unsafe_get_uint16_le : ('a rd, 'k) t -> ('k, [le | unsigned] int16) get
val unsafe_get_uint16_be : ('a rd, 'k) t -> ('k, [be | unsigned] int16) get
val unsafe_get_int16_le : ('a rd, 'k) t -> ('k, le int16) get
val unsafe_get_int16_be : ('a rd, 'k) t -> ('k, be int16) get
val unsafe_get_int32_le : ('a rd, 'k) t -> ('k, le int32) get
val unsafe_get_int32_be : ('a rd, 'k) t -> ('k, be int32) get
val unsafe_get_int64_le : ('a rd, 'k) t -> ('k, le int64) get
val unsafe_get_int64_be : ('a rd, 'k) t -> ('k, be int64) get
val unsafe_set : ('a wr, 'k) t -> ('k, char) set
val unsafe_set_int16_le : ('a wr, 'k) t -> ('k, le int16) set
val unsafe_set_int16_be : ('a wr, 'k) t -> ('k, be int16) set
val unsafe_set_int32_le : ('a wr, 'k) t -> ('k, le int32) set
val unsafe_set_int32_be : ('a wr, 'k) t -> ('k, be int32) set
val unsafe_set_int64_le : ('a wr, 'k) t -> ('k, le int64) set
val unsafe_set_int64_be : ('a wr, 'k) t -> ('k, be int64) set
