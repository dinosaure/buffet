open S
open Buffet0

type bytes = Bytes.t
type string = String.t
type bigstring = Bigstring.t

type 'k tag =
  | Bytes : bytes tag
  | String : string tag
  | Bigstring : bigstring tag

let bytes = Bytes
let string = String
let bigstring = Bigstring

type value = Bytes of bytes | String of string | Bigstring of bigstring

let make : type k. k tag -> int -> char -> k =
 fun witness len chr ->
  match witness with
  | Bytes -> Bytes.make len chr
  | String -> String.make len chr
  | Bigstring -> Bigstring.make len chr

let empty : type k. k tag -> k = function
  | Bytes -> Bytes.empty
  | String -> String.empty
  | Bigstring -> Bigstring.empty

let unsafe_copy : type k. k tag -> (k, k) copy =
 fun witness buf ->
  match witness with
  | Bytes -> Bytes.unsafe_copy buf
  | String -> String.unsafe_copy buf
  | Bigstring -> Bigstring.unsafe_copy buf

let copy : type k. k tag -> (k, k) copy =
 fun witness buf ->
  match witness with
  | Bytes -> Bytes.copy buf
  | String -> String.copy buf
  | Bigstring -> Bigstring.copy buf

let unsafe_sub : type k. k tag -> (k, k) sub =
 fun witness buf ~off ~len ->
  match witness with
  | Bytes -> Bytes.unsafe_sub buf ~off ~len
  | String -> String.unsafe_sub buf ~off ~len
  | Bigstring -> Bigstring.unsafe_sub buf ~off ~len

let sub : type k. k tag -> (k, k) sub =
 fun witness buf ~off ~len ->
  match witness with
  | Bytes -> Bytes.sub buf ~off ~len
  | String -> String.sub buf ~off ~len
  | Bigstring -> Bigstring.sub buf ~off ~len

let create : type k. k tag -> int -> k =
 fun witness len ->
  match witness with
  | Bytes -> Bytes.create len
  | Bigstring -> Bigstring.create len
  | String -> invalid_arg "create unavailable on string"

let length : type k. k tag -> k length =
 fun witness buf ->
  match witness with
  | Bytes -> Bytes.length buf
  | String -> String.length buf
  | Bigstring -> Bigstring.length buf

let unsafe_get : type k. k tag -> (k, char) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.unsafe_get buf pos
  | String -> String.unsafe_get buf pos
  | Bigstring -> Bigstring.unsafe_get buf pos

let unsafe_set : type k. k tag -> (k, char) set =
 fun witness buf pos chr ->
  match witness with
  | Bytes -> Bytes.unsafe_set buf pos chr
  | Bigstring -> Bigstring.unsafe_set buf pos chr
  | String -> invalid_arg "unsafe_set unavailable on string"

let get : type k. k tag -> (k, char) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get buf pos
  | String -> buf.[pos]
  | Bigstring -> Bigstring.get buf pos

let set : type k. k tag -> (k, char) set =
 fun witness buf pos chr ->
  match witness with
  | Bytes -> Bytes.set buf pos chr
  | Bigstring -> Bigstring.set buf pos chr
  | String -> invalid_arg "set unavailable on string"

let pp : type k. k tag -> k fmt =
 fun witness ppf buf ->
  match witness with
  | Bytes -> Bytes.pp ppf buf
  | String -> String.pp ppf buf
  | Bigstring -> Bigstring.pp ppf buf

let unsafe_sub_pp : type k. k tag -> off:int -> len:int -> k fmt =
 fun witness ~off ~len ppf buf ->
  match witness with
  | Bytes -> Bytes.unsafe_sub_pp ~off ~len ppf buf
  | String -> String.unsafe_sub_pp ~off ~len ppf buf
  | Bigstring -> Bigstring.unsafe_sub_pp ~off ~len ppf buf

let sub_pp : type k. k tag -> off:int -> len:int -> k fmt =
 fun witness ~off ~len ppf buf ->
  match witness with
  | Bytes -> Bytes.sub_pp ~off ~len ppf buf
  | String -> String.sub_pp ~off ~len ppf buf
  | Bigstring -> Bigstring.sub_pp ~off ~len ppf buf

let compare : type k. k tag -> k compare =
 fun witness a b ->
  match witness with
  | Bytes -> Bytes.compare a b
  | String -> String.compare a b
  | Bigstring -> Bigstring.compare a b

let unsafe_sub_compare : type k. k tag -> a:slice -> b:slice -> k compare =
  function
  | Bytes -> Bytes.unsafe_sub_compare
  | String -> String.unsafe_sub_compare
  | Bigstring -> Bigstring.unsafe_sub_compare

let sub_compare : type k. k tag -> a:slice -> b:slice -> k compare = function
  | Bytes -> Bytes.sub_compare
  | String -> String.sub_compare
  | Bigstring -> Bigstring.sub_compare

let equal : type k. k tag -> k equal = function
  | Bytes -> Bytes.equal
  | String -> String.equal
  | Bigstring -> Bigstring.equal

let unsafe_sub_equal : type k. k tag -> a:slice -> b:slice -> k equal =
  function
  | Bytes -> Bytes.unsafe_sub_equal
  | String -> String.unsafe_sub_equal
  | Bigstring -> Bigstring.unsafe_sub_equal

let sub_equal : type k. k tag -> a:slice -> b:slice -> k equal = function
  | Bytes -> Bytes.sub_equal
  | String -> String.sub_equal
  | Bigstring -> Bigstring.sub_equal

let to_value : type k. k tag -> k -> value = function
  | Bytes -> fun x -> Bytes x
  | String -> fun x -> String x
  | Bigstring -> fun x -> Bigstring x

type v = V : 'k tag * 'k -> v

let of_value : value -> v = function
  | Bytes x -> V (Bytes, x)
  | String x -> V (String, x)
  | Bigstring x -> V (Bigstring, x)

module Tag = struct
  type 'k t = 'k tag

  let equal : type a b. a tag -> b tag -> (a, b) Refl.t option =
   fun a b ->
    match (a, b) with
    | Bytes, Bytes -> Some Refl.Refl
    | String, String -> Some Refl.Refl
    | Bigstring, Bigstring -> Some Refl.Refl
    | _a, _b -> None
end

let coerce : type k. value -> k tag -> (k tag * k) option =
 fun v t ->
  let (V (t', v')) = of_value v in
  match Tag.equal t t' with Some Refl.Refl -> Some (t, v') | None -> None
