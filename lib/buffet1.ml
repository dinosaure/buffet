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

let make : type k. k tag -> int -> char -> k = function
  | Bytes -> Bytes.make
  | String -> String.make
  | Bigstring -> Bigstring.make

let empty : type k. k tag -> k = function
  | Bytes -> Bytes.empty
  | String -> String.empty
  | Bigstring -> Bigstring.empty

let unsafe_copy : type k. k tag -> (k, k) copy = function
  | Bytes -> Bytes.unsafe_copy
  | String -> String.unsafe_copy
  | Bigstring -> Bigstring.unsafe_copy

let copy : type k. k tag -> (k, k) copy = function
  | Bytes -> Bytes.copy
  | String -> String.copy
  | Bigstring -> Bigstring.copy

let unsafe_sub : type k. k tag -> (k, k) sub = function
  | Bytes -> Bytes.unsafe_sub
  | String -> String.unsafe_sub
  | Bigstring -> Bigstring.unsafe_sub

let sub : type k. k tag -> (k, k) sub = function
  | Bytes -> Bytes.sub
  | String -> String.sub
  | Bigstring -> Bigstring.sub

let create : type k. k tag -> int -> k = function
  | Bytes -> Bytes.create
  | Bigstring -> Bigstring.create
  | String -> invalid_arg "create unavailable on string"

let length : type k. k tag -> k length = function
  | Bytes -> Bytes.length
  | String -> String.length
  | Bigstring -> Bigstring.length

let unsafe_get : type k. k tag -> (k, char) get = function
  | Bytes -> Bytes.unsafe_get
  | String -> String.unsafe_get
  | Bigstring -> Bigstring.unsafe_get

let unsafe_set : type k. k tag -> (k, char) set = function
  | Bytes -> Bytes.unsafe_set
  | Bigstring -> Bigstring.unsafe_set
  | String -> invalid_arg "unsafe_set unavailable on string"

let get : type k. k tag -> (k, char) get = function
  | Bytes -> Bytes.get
  | String -> String.get
  | Bigstring -> Bigstring.get

let set : type k. k tag -> (k, char) set = function
  | Bytes -> Bytes.set
  | Bigstring -> Bigstring.set
  | String -> invalid_arg "set unavailable on string"

let pp : type k. k tag -> k fmt = function
  | Bytes -> Bytes.pp
  | String -> String.pp
  | Bigstring -> Bigstring.pp

let unsafe_sub_pp : type k. k tag -> off:int -> len:int -> k fmt = function
  | Bytes -> Bytes.unsafe_sub_pp
  | String -> String.unsafe_sub_pp
  | Bigstring -> Bigstring.unsafe_sub_pp

let sub_pp : type k. k tag -> off:int -> len:int -> k fmt = function
  | Bytes -> Bytes.sub_pp
  | String -> String.sub_pp
  | Bigstring -> Bigstring.sub_pp

let compare : type k. k tag -> k compare = function
  | Bytes -> Bytes.compare
  | String -> String.compare
  | Bigstring -> Bigstring.compare

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
