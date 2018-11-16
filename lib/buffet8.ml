open Buffet6

type (-'a, 'k) witness =
  | Bytes : (_, [rd | wr] Bytes.t) witness
  | String : (_, rd String.t) witness
  | Bigstring : (_, [rd | wr | async] Bigstring.t) witness

let bytes : (([rd | wr] as 'a), 'a Bytes.t) witness = Bytes
let string : ((rd as 'a), 'a String.t) witness = String
let bigstring : (([rd | wr | async] as 'a), 'a Bigstring.t) witness = Bigstring

let create : type k. ('a, k) witness -> int -> k =
 fun witness len ->
  match witness with
  | Bytes -> Bytes.create len
  | Bigstring -> Bigstring.create len
  | String -> invalid_arg "Can not create a string"

let make : type k. ('a, k) witness -> int -> char -> k =
 fun witness len chr ->
  match witness with
  | Bytes -> Bytes.make len chr
  | String -> String.make len chr
  | Bigstring -> Bigstring.make len chr

let get : type k. ([> rd], k) witness -> k -> int -> char =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get buf pos
  | String -> buf.[pos]
  | Bigstring -> Bigstring.get buf pos
