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

open Integer

let unsafe_get_uint8 : type k. k tag -> (k, unsigned int8) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.unsafe_get_uint8 buf pos
  | String -> String.unsafe_get_uint8 buf pos
  | Bigstring -> Bigstring.unsafe_get_uint8 buf pos

let get_uint8 : type k. k tag -> (k, unsigned int8) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_uint8 buf pos
  | String -> String.get_uint8 buf pos
  | Bigstring -> Bigstring.get_uint8 buf pos

let unsafe_get_int8 : type k. k tag -> (k, signed int8) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.unsafe_get_int8 buf pos
  | String -> String.unsafe_get_int8 buf pos
  | Bigstring -> Bigstring.unsafe_get_int8 buf pos

let get_int8 : type k. k tag -> (k, signed int8) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_int8 buf pos
  | String -> String.get_int8 buf pos
  | Bigstring -> Bigstring.get_int8 buf pos

let unsafe_set_int8 : type k. k tag -> (k, signed int8) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.unsafe_set_int8 buf pos x
  | Bigstring -> Bigstring.unsafe_set_int8 buf pos x
  | String -> invalid_arg "set unavailable on string"

let set_int8 : type k. k tag -> (k, signed int8) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.set_int8 buf pos x
  | Bigstring -> Bigstring.set_int8 buf pos x
  | String -> invalid_arg "set unavailable on string"

let unsafe_get_uint16_le : type k. k tag -> (k, [le | unsigned] int16) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.unsafe_get_uint16_le buf pos
  | String -> String.unsafe_get_uint16_le buf pos
  | Bigstring -> Bigstring.unsafe_get_uint16_le buf pos

let unsafe_get_uint16_be : type k. k tag -> (k, [be | unsigned] int16) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.unsafe_get_uint16_be buf pos
  | String -> String.unsafe_get_uint16_be buf pos
  | Bigstring -> Bigstring.unsafe_get_uint16_be buf pos

let get_uint16_le : type k. k tag -> (k, [le | unsigned] int16) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_uint16_le buf pos
  | String -> String.get_uint16_le buf pos
  | Bigstring -> Bigstring.get_uint16_le buf pos

let get_uint16_be : type k. k tag -> (k, [be | unsigned] int16) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_uint16_be buf pos
  | String -> String.get_uint16_be buf pos
  | Bigstring -> Bigstring.get_uint16_be buf pos

let unsafe_get_int16_le : type k. k tag -> (k, le int16) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.unsafe_get_int16_le buf pos
  | String -> String.unsafe_get_int16_le buf pos
  | Bigstring -> Bigstring.unsafe_get_int16_le buf pos

let unsafe_get_int32_le : type k. k tag -> (k, le int32) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.unsafe_get_int32_le buf pos
  | String -> String.unsafe_get_int32_le buf pos
  | Bigstring -> Bigstring.unsafe_get_int32_le buf pos

let unsafe_get_int64_le : type k. k tag -> (k, le int64) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.unsafe_get_int64_le buf pos
  | String -> String.unsafe_get_int64_le buf pos
  | Bigstring -> Bigstring.unsafe_get_int64_le buf pos

let unsafe_get_int16_be : type k. k tag -> (k, be int16) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.unsafe_get_int16_be buf pos
  | String -> String.unsafe_get_int16_be buf pos
  | Bigstring -> Bigstring.unsafe_get_int16_be buf pos

let unsafe_get_int32_be : type k. k tag -> (k, be int32) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.unsafe_get_int32_be buf pos
  | String -> String.unsafe_get_int32_be buf pos
  | Bigstring -> Bigstring.unsafe_get_int32_be buf pos

let unsafe_get_int64_be : type k. k tag -> (k, be int64) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.unsafe_get_int64_be buf pos
  | String -> String.unsafe_get_int64_be buf pos
  | Bigstring -> Bigstring.unsafe_get_int64_be buf pos

let get_int16_le : type k. k tag -> (k, le int16) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_int16_le buf pos
  | String -> String.get_int16_le buf pos
  | Bigstring -> Bigstring.get_int16_le buf pos

let get_int32_le : type k. k tag -> (k, le int32) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_int32_le buf pos
  | String -> String.get_int32_le buf pos
  | Bigstring -> Bigstring.get_int32_le buf pos

let get_int64_le : type k. k tag -> (k, le int64) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_int64_le buf pos
  | String -> String.get_int64_le buf pos
  | Bigstring -> Bigstring.get_int64_le buf pos

let get_int16_be : type k. k tag -> (k, be int16) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_int16_be buf pos
  | String -> String.get_int16_be buf pos
  | Bigstring -> Bigstring.get_int16_be buf pos

let get_int32_be : type k. k tag -> (k, be int32) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_int32_be buf pos
  | String -> String.get_int32_be buf pos
  | Bigstring -> Bigstring.get_int32_be buf pos

let get_int64_be : type k. k tag -> (k, be int64) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_int64_be buf pos
  | String -> String.get_int64_be buf pos
  | Bigstring -> Bigstring.get_int64_be buf pos

let unsafe_set_int16_le : type k. k tag -> (k, le int16) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.unsafe_set_int16_le buf pos x
  | Bigstring -> Bigstring.unsafe_set_int16_le buf pos x
  | String -> invalid_arg "unsafe_set_int16_le unavailable on string"

let unsafe_set_int16_be : type k. k tag -> (k, be int16) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.unsafe_set_int16_be buf pos x
  | Bigstring -> Bigstring.unsafe_set_int16_be buf pos x
  | String -> invalid_arg "unsafe_set_int16_be unavailable on string"

let unsafe_set_int32_le : type k. k tag -> (k, le int32) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.unsafe_set_int32_le buf pos x
  | Bigstring -> Bigstring.unsafe_set_int32_be buf pos x
  | String -> invalid_arg "unsafe_set_int32_le unavailable on string"

let unsafe_set_int32_be : type k. k tag -> (k, be int32) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.unsafe_set_int32_be buf pos x
  | Bigstring -> Bigstring.unsafe_set_int32_be buf pos x
  | String -> invalid_arg "unsafe_set_int32_be unavailable on string"

let unsafe_set_int64_le : type k. k tag -> (k, le int64) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.unsafe_set_int64_le buf pos x
  | Bigstring -> Bigstring.unsafe_set_int64_le buf pos x
  | String -> invalid_arg "unsafe_set_int64_le unavailable on string"

let unsafe_set_int64_be : type k. k tag -> (k, be int64) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.unsafe_set_int64_be buf pos x
  | Bigstring -> Bigstring.unsafe_set_int64_be buf pos x
  | String -> invalid_arg "unsafe_set_int64_be unavailable on string"

let set_int16_le : type k. k tag -> (k, le int16) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.set_int16_le buf pos x
  | Bigstring -> Bigstring.set_int16_le buf pos x
  | String -> invalid_arg "set_int16_le unavailable on string"

let set_int16_be : type k. k tag -> (k, be int16) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.set_int16_be buf pos x
  | Bigstring -> Bigstring.set_int16_be buf pos x
  | String -> invalid_arg "set_int16_be unavailable on string"

let set_int32_le : type k. k tag -> (k, le int32) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.set_int32_le buf pos x
  | Bigstring -> Bigstring.set_int32_le buf pos x
  | String -> invalid_arg "set_int32_le unavailable on string"

let set_int32_be : type k. k tag -> (k, be int32) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.set_int32_be buf pos x
  | Bigstring -> Bigstring.set_int32_be buf pos x
  | String -> invalid_arg "set_int32_be unavailable on string"

let set_int64_le : type k. k tag -> (k, le int64) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.set_int64_le buf pos x
  | Bigstring -> Bigstring.set_int64_le buf pos x
  | String -> invalid_arg "set_int64_le unavailable on string"

let set_int64_be : type k. k tag -> (k, be int64) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.set_int64_be buf pos x
  | Bigstring -> Bigstring.set_int64_be buf pos x
  | String -> invalid_arg "set_int64_be unavailable on string"

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
 fun witness ~a ~b x y ->
  match witness with
  | Bytes -> Bytes.unsafe_sub_compare ~a ~b x y
  | String -> String.unsafe_sub_compare ~a ~b x y
  | Bigstring -> Bigstring.unsafe_sub_compare ~a ~b x y

let sub_compare : type k. k tag -> a:slice -> b:slice -> k compare =
 fun witness ~a ~b x y ->
  match witness with
  | Bytes -> Bytes.sub_compare ~a ~b x y
  | String -> String.sub_compare ~a ~b x y
  | Bigstring -> Bigstring.sub_compare ~a ~b x y

let equal : type k. k tag -> k equal =
 fun witness a b ->
  match witness with
  | Bytes -> Bytes.equal a b
  | String -> String.equal a b
  | Bigstring -> Bigstring.equal a b

let unsafe_sub_equal : type k. k tag -> a:slice -> b:slice -> k equal =
 fun witness ~a ~b x y ->
  match witness with
  | Bytes -> Bytes.unsafe_sub_equal ~a ~b x y
  | String -> String.unsafe_sub_equal ~a ~b x y
  | Bigstring -> Bigstring.unsafe_sub_equal ~a ~b x y

let sub_equal : type k. k tag -> a:slice -> b:slice -> k equal =
 fun witness ~a ~b x y ->
  match witness with
  | Bytes -> Bytes.sub_equal ~a ~b x y
  | String -> String.sub_equal ~a ~b x y
  | Bigstring -> Bigstring.sub_equal ~a ~b x y

let to_value : type k. k tag -> k -> value =
 fun witness x ->
  match witness with
  | Bytes -> Bytes x
  | String -> String x
  | Bigstring -> Bigstring x

type v = V : 'k tag * 'k -> v

let of_value : value -> v = function
  | Bytes x -> V (Bytes, x)
  | String x -> V (String, x)
  | Bigstring x -> V (Bigstring, x)

module Tag = struct
  type 'k t = 'k tag

  let some_refl = Some Refl.Refl
  let none = None

  let equal : type a b. a tag -> b tag -> (a, b) Refl.t option =
   fun a b ->
    match (a, b) with
    | Bytes, Bytes -> some_refl
    | String, String -> some_refl
    | Bigstring, Bigstring -> some_refl
    | _a, _b -> none
end

let coerce : type k. value -> k tag -> (k tag * k) option =
 fun v t ->
  let (V (t', v')) = of_value v in
  match Tag.equal t t' with Some Refl.Refl -> Some (t, v') | None -> None
