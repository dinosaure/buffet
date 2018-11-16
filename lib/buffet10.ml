open Buffet9

type ('a, 'k) witness =
  | Bytes : ('a, 'a Bytes.t) witness
  | String : ('a, 'a String.t) witness
  | Bigstring : ('a, 'a Bigstring.t) witness

let bytes : ('a, 'a Bytes.t) witness = Bytes
let string : ('a, 'a String.t) witness = String
let bigstring : ('a, 'a Bigstring.t) witness = Bigstring
