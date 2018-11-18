open Buffet1

type 'a rd = < rd: unit ; .. > as 'a
type 'a wr = < wr: unit ; .. > as 'a
type 'a async = < async: unit ; .. > as 'a
type 'a aligned = < aligned: unit ; .. > as 'a

(* / *)

type ('a, 'k) t = 'k tag

let bytes : ('a, bytes) t = bytes
let string : ('a, string) t = string
let bigstring : ('a, bigstring) t = bigstring

let create : type k. ('a wr, k) t -> int -> k =
 fun witness len -> create witness len

let make : type k. ('a rd, k) t -> int -> char -> k =
 fun witness len chr -> make witness len chr

let get : type k. ('a rd, k) t -> k -> int -> char =
 fun witness buf off -> get witness buf off

let unsafe_get : type k. ('a rd, k) t -> k -> int -> char =
 fun witness buf off -> unsafe_get witness buf off

let set : type k. ('a wr, k) t -> k -> int -> char -> unit =
 fun witness buf off chr -> set witness buf off chr

let unsafe_set : type k. ('a wr, k) t -> k -> int -> char -> unit =
 fun witness buf off chr -> unsafe_set witness buf off chr
