open S
open Integer
open Buffet0
open Buffet1

(* / *)

module Bool = struct type t = True type f = False end

type ('rd, 'wr, 'async) c = < rd: 'rd ; wr: 'wr ; async: 'async >
type 'async ro = (Bool.t, Bool.f, 'async) c
type 'async wo = (Bool.f, Bool.t, 'async) c
type 'async rdwr = (Bool.t, Bool.t, 'async) c

type ('a, 'k) access =
  | Bytes : (Bool.f rdwr, bytes) access
  | String : (Bool.f ro, string) access
  | Bigstring : (Bool.t rdwr, bigstring) access

let bytes = Bytes
let string = String
let bigstring = Bigstring

let make : type a k. (a, k) access -> int -> char -> k =
 fun witness len chr ->
  match witness with
  | Bytes -> Bytes.make len chr
  | String -> String.make len chr
  | Bigstring -> Bigstring.make len chr

let empty : type a k. (a, k) access -> k = function
  | Bytes -> Bytes.empty
  | String -> String.empty
  | Bigstring -> Bigstring.empty

let unsafe_copy : type k wr async.
    ((Bool.t, wr, async) c, k) access -> (k, k) copy =
 fun witness buf ->
  match witness with
  | String -> String.unsafe_copy buf
  | Bytes -> Bytes.unsafe_copy buf
  | Bigstring -> Bigstring.unsafe_copy buf

let copy : type k wr async. ((Bool.t, wr, async) c, k) access -> (k, k) copy =
 fun witness buf ~off ~len ->
  match witness with
  | Bytes -> Bytes.copy buf ~off ~len
  | String -> String.copy buf ~off ~len
  | Bigstring -> Bigstring.copy buf ~off ~len

let unsafe_sub : type k wr async.
    ((Bool.t, wr, async) c, k) access -> (k, k) sub =
 fun witness buf ~off ~len ->
  match witness with
  | Bytes -> Bytes.unsafe_sub buf ~off ~len
  | String -> String.unsafe_sub buf ~off ~len
  | Bigstring -> Bigstring.unsafe_sub buf ~off ~len

let sub : type k wr async. ((Bool.t, wr, async) c, k) access -> (k, k) sub =
 fun witness buf ~off ~len ->
  match witness with
  | Bytes -> Bytes.sub buf ~off ~len
  | String -> String.sub buf ~off ~len
  | Bigstring -> Bigstring.sub buf ~off ~len

let create : type k rd async. ((rd, Bool.t, async) c, k) access -> int -> k =
 fun witness len ->
  match witness with
  | Bytes -> Bytes.create len
  | Bigstring -> Bigstring.create len

let length : type a k. (a, k) access -> k -> int =
 fun witness buf ->
  match witness with
  | Bytes -> Bytes.length buf
  | String -> String.length buf
  | Bigstring -> Bigstring.length buf

let unsafe_get : type k wr async.
    ((Bool.t, wr, async) c, k) access -> (k, char) get =
 fun witness buf off ->
  match witness with
  | Bytes -> Bytes.unsafe_get buf off
  | String -> String.unsafe_get buf off
  | Bigstring -> Bigstring.unsafe_get buf off

let unsafe_set : type k rd async.
    ((rd, Bool.t, async) c, k) access -> (k, char) set =
 fun witness buf off chr ->
  match witness with
  | Bytes -> Bytes.unsafe_set buf off chr
  | Bigstring -> Bigstring.unsafe_set buf off chr

let get : type k wr async. ((Bool.t, wr, async) c, k) access -> (k, char) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get buf pos
  | String -> buf.[pos]
  | Bigstring -> Bigstring.get buf pos

let get_uint16_le : type k wr async.
    ((Bool.t, wr, async) c, k) access -> (k, [le | unsigned] int16) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_uint16_le buf pos
  | String -> String.get_uint16_le buf pos
  | Bigstring -> Bigstring.get_uint16_le buf pos

let get_int16_le : type k wr async.
    ((Bool.t, wr, async) c, k) access -> (k, le int16) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_int16_le buf pos
  | String -> String.get_int16_le buf pos
  | Bigstring -> Bigstring.get_int16_le buf pos

let set_int16_le : type k rd async.
    ((rd, Bool.t, async) c, k) access -> (k, le int16) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.set_int16_le buf pos x
  | Bigstring -> Bigstring.set_int16_le buf pos x

let get_int32_le : type k wr async.
    ((Bool.t, wr, async) c, k) access -> (k, le int32) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_int32_le buf pos
  | String -> String.get_int32_le buf pos
  | Bigstring -> Bigstring.get_int32_le buf pos

let set_int32_le : type k rd async.
    ((rd, Bool.t, async) c, k) access -> (k, le int32) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.set_int32_le buf pos x
  | Bigstring -> Bigstring.set_int32_le buf pos x

let get_int64_le : type k wr async.
    ((Bool.t, wr, async) c, k) access -> (k, le int64) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_int64_le buf pos
  | String -> String.get_int64_le buf pos
  | Bigstring -> Bigstring.get_int64_le buf pos

let set_int64_le : type k rd async.
    ((rd, Bool.t, async) c, k) access -> (k, le int64) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.set_int64_le buf pos x
  | Bigstring -> Bigstring.set_int64_le buf pos x

let get_uint16_be : type k wr async.
    ((Bool.t, wr, async) c, k) access -> (k, [be | unsigned] int16) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_uint16_be buf pos
  | String -> String.get_uint16_be buf pos
  | Bigstring -> Bigstring.get_uint16_be buf pos

let get_int16_be : type k wr async.
    ((Bool.t, wr, async) c, k) access -> (k, be int16) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_int16_be buf pos
  | String -> String.get_int16_be buf pos
  | Bigstring -> Bigstring.get_int16_be buf pos

let set_int16_be : type k rd async.
    ((rd, Bool.t, async) c, k) access -> (k, be int16) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.set_int16_be buf pos x
  | Bigstring -> Bigstring.set_int16_be buf pos x

let get_int32_be : type k wr async.
    ((Bool.t, wr, async) c, k) access -> (k, be int32) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_int32_be buf pos
  | String -> String.get_int32_be buf pos
  | Bigstring -> Bigstring.get_int32_be buf pos

let set_int32_be : type k rd async.
    ((rd, Bool.t, async) c, k) access -> (k, be int32) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.set_int32_be buf pos x
  | Bigstring -> Bigstring.set_int32_be buf pos x

let get_int64_be : type k wr async.
    ((Bool.t, wr, async) c, k) access -> (k, be int64) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get_int64_be buf pos
  | String -> String.get_int64_be buf pos
  | Bigstring -> Bigstring.get_int64_be buf pos

let set_int64_be : type k rd async.
    ((rd, Bool.t, async) c, k) access -> (k, be int64) set =
 fun witness buf pos x ->
  match witness with
  | Bytes -> Bytes.set_int64_be buf pos x
  | Bigstring -> Bigstring.set_int64_be buf pos x

let set : type k rd async. ((rd, Bool.t, async) c, k) access -> (k, char) set =
 fun witness buf off chr ->
  match witness with
  | Bytes -> Bytes.set buf off chr
  | Bigstring -> Bigstring.set buf off chr

let pp : type k wr async. ((Bool.t, wr, async) c, k) access -> k fmt =
 fun witness ppf buf ->
  match witness with
  | Bytes -> Bytes.pp ppf buf
  | String -> String.pp ppf buf
  | Bigstring -> Bigstring.pp ppf buf

let unsafe_sub_pp : type k wr async.
    ((Bool.t, wr, async) c, k) access -> off:int -> len:int -> k fmt =
 fun witness ~off ~len ppf buf ->
  match witness with
  | Bytes -> Bytes.unsafe_sub_pp ~off ~len ppf buf
  | String -> String.unsafe_sub_pp ~off ~len ppf buf
  | Bigstring -> Bigstring.unsafe_sub_pp ~off ~len ppf buf

let sub_pp : type k wr async.
    ((Bool.t, wr, async) c, k) access -> off:int -> len:int -> k fmt =
 fun witness ~off ~len ppf buf ->
  match witness with
  | Bytes -> Bytes.sub_pp ~off ~len ppf buf
  | String -> String.sub_pp ~off ~len ppf buf
  | Bigstring -> Bigstring.sub_pp ~off ~len ppf buf

let compare : type k wr async. ((Bool.t, wr, async) c, k) access -> k compare =
 fun witness a b ->
  match witness with
  | Bytes -> Bytes.compare a b
  | String -> String.compare a b
  | Bigstring -> Bigstring.compare a b

let unsafe_sub_compare : type k wr async.
    ((Bool.t, wr, async) c, k) access -> a:slice -> b:slice -> k compare =
 fun witness ~a ~b x y ->
  match witness with
  | Bytes -> Bytes.unsafe_sub_compare ~a ~b x y
  | String -> String.unsafe_sub_compare ~a ~b x y
  | Bigstring -> Bigstring.unsafe_sub_compare ~a ~b x y

let sub_compare : type k wr async.
    ((Bool.t, wr, async) c, k) access -> a:slice -> b:slice -> k compare =
 fun witness ~a ~b x y ->
  match witness with
  | Bytes -> Bytes.sub_compare ~a ~b x y
  | String -> String.sub_compare ~a ~b x y
  | Bigstring -> Bigstring.sub_compare ~a ~b x y

let equal : type k wr async. ((Bool.t, wr, async) c, k) access -> k equal =
 fun witness a b ->
  match witness with
  | Bytes -> Bytes.equal a b
  | String -> String.equal a b
  | Bigstring -> Bigstring.equal a b

let unsafe_sub_equal : type k wr async.
    ((Bool.t, wr, async) c, k) access -> a:slice -> b:slice -> k equal =
 fun witness ~a ~b x y ->
  match witness with
  | Bytes -> Bytes.unsafe_sub_equal ~a ~b x y
  | String -> String.unsafe_sub_equal ~a ~b x y
  | Bigstring -> Bigstring.unsafe_sub_equal ~a ~b x y

let sub_equal : type k wr async.
    ((Bool.t, wr, async) c, k) access -> a:slice -> b:slice -> k equal =
 fun witness ~a ~b x y ->
  match witness with
  | Bytes -> Bytes.sub_equal ~a ~b x y
  | String -> String.sub_equal ~a ~b x y
  | Bigstring -> Bigstring.sub_equal ~a ~b x y

module Access = struct
  type ('a, 'k) t = ('a, 'k) access

  let some_refl = Some Refl.Refl
  let none = None

  let equal : type a0 a1 k0 k1.
      (a0, k0) t -> (a1, k1) t -> (k0, k1) Refl.t option =
   fun a b ->
    match (a, b) with
    | Bytes, Bytes -> some_refl
    | String, String -> some_refl
    | Bigstring, Bigstring -> some_refl
    | _a, _b -> none

  let read : type k rd wr async.
      ((rd, wr, async) c, k) t -> (rd, Bool.t) Refl.t option = function
    | Bytes -> some_refl
    | String -> some_refl
    | Bigstring -> some_refl

  let write : type k rd wr async.
      ((rd, wr, async) c, k) t -> (wr, Bool.t) Refl.t option = function
    | Bytes -> some_refl
    | Bigstring -> some_refl
    | String -> none

  let async : type k rd wr async.
      ((rd, wr, async) c, k) t -> (async, Bool.t) Refl.t option = function
    | Bigstring -> some_refl
    | Bytes -> none
    | String -> none
end

let coerce : type a k. (a, k) access -> k tag = function
  | Bytes -> Buffet1.bytes
  | String -> Buffet1.string
  | Bigstring -> Buffet1.bigstring
