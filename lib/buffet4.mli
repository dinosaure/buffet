open S
open Buffet1
open Buffet3

type t

val of_string : ?off:int -> ?len:int -> string -> t
val of_bytes : ?off:int -> ?len:int -> bytes -> t
val of_bigstring : ?off:int -> ?len:int -> bigstring -> t
val copy : (t, t) copy
val sub : (t, t) sub
val length : t length
val unsafe_get : (t, char) get
val unsafe_set : (t, char) set
val get : (t, char) get
val set : (t, char) set
val pp : t fmt
val compare : t compare
val equal : t equal
