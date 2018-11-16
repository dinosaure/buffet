open Buffet6

type (-'a, 'k) witness

val string : ((rd as 'a), 'a String.t) witness
val bytes : (([rd | wr] as 'a), 'a Bytes.t) witness
val bigstring : (([rd | wr | async] as 'a), 'a Bigstring.t) witness

(* / *)

val create : ('a, 'k) witness -> int -> 'k
val make : ('a, 'k) witness -> int -> char -> 'k
val get : ([> rd], 'k) witness -> 'k -> int -> char
val unsafe_get : ([> rd], 'k) witness -> 'k -> int -> char
