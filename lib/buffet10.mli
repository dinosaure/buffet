open Buffet1

type 'a rd = < rd: unit ; .. > as 'a
type 'a wr = < wr: unit ; .. > as 'a
type 'a async = < async: unit ; .. > as 'a
type 'a aligned = < aligned: unit ; .. > as 'a

(* / *)

type ('a, 'k) t

val bytes : (< rd: unit ; wr: unit >, bytes) t
val string : (< rd: unit >, string) t
val bigstring : (< rd: unit ; wr: unit ; async: unit >, bigstring) t

(* / *)

val create : ('a wr, 'k) t -> int -> 'k
val make : ('a rd, 'k) t -> int -> char -> 'k
val get : ('a rd, 'k) t -> 'k -> int -> char
val unsafe_get : ('a rd, 'k) t -> 'k -> int -> char
val set : ('a wr, 'k) t -> 'k -> int -> char -> unit
val unsafe_set : ('a wr, 'k) t -> 'k -> int -> char -> unit
