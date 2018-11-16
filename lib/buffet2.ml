open S
open Buffet0
open Buffet1

type ('rd, 'wr, 'async) c = < rd: 'rd ; wr: 'wr ; async: 'async >
type t = [`True]
type f = [`False]
type 'async ro = (t, f, 'async) c
type 'async wo = (f, t, 'async) c
type 'async rdwr = (t, t, 'async) c

type ('a, 'k) access =
  | Bytes : (f rdwr, bytes) access
  | String : (f ro, string) access
  | Bigstring : (t rdwr, bigstring) access

let bytes = Bytes
let string = String
let bigstring = Bigstring

let make : type a k. (a, k) access -> int -> char -> k = function
  | Bytes -> Bytes.make
  | String -> String.make
  | Bigstring -> Bigstring.make

let empty : type a k. (a, k) access -> k = function
  | Bytes -> Bytes.empty
  | String -> String.empty
  | Bigstring -> Bigstring.empty

let unsafe_copy : type k wr async. ((t, wr, async) c, k) access -> (k, k) copy
    = function
  | String -> String.unsafe_copy
  | Bytes -> Bytes.unsafe_copy
  | Bigstring -> Bigstring.unsafe_copy

let copy : type k wr async. ((t, wr, async) c, k) access -> (k, k) copy =
  function
  | Bytes -> Bytes.copy
  | String -> String.copy
  | Bigstring -> Bigstring.copy

let unsafe_sub : type k wr async. ((t, wr, async) c, k) access -> (k, k) sub =
  function
  | Bytes -> Bytes.unsafe_sub
  | String -> String.unsafe_sub
  | Bigstring -> Bigstring.unsafe_sub

let sub : type k wr async. ((t, wr, async) c, k) access -> (k, k) sub =
  function
  | Bytes -> Bytes.sub
  | String -> String.sub
  | Bigstring -> Bigstring.sub

let create : type k rd async. ((rd, t, async) c, k) access -> int -> k =
  function
  | Bytes -> Bytes.create
  | Bigstring -> Bigstring.create

let length : type a k. (a, k) access -> k -> int = function
  | Bytes -> Bytes.length
  | String -> String.length
  | Bigstring -> Bigstring.length

let unsafe_get : type k wr async. ((t, wr, async) c, k) access -> (k, char) get
    = function
  | Bytes -> Bytes.unsafe_get
  | String -> String.unsafe_get
  | Bigstring -> Bigstring.unsafe_get

let unsafe_set : type k rd async. ((rd, t, async) c, k) access -> (k, char) set
    = function
  | Bytes -> Bytes.unsafe_set
  | Bigstring -> Bigstring.unsafe_set

let get : type k wr async. ((t, wr, async) c, k) access -> (k, char) get =
 fun witness buf pos ->
  match witness with
  | Bytes -> Bytes.get buf pos
  | String -> buf.[pos]
  | Bigstring -> Bigstring.get buf pos

let set : type k rd async. ((rd, t, async) c, k) access -> (k, char) set =
  function
  | Bytes -> Bytes.set
  | Bigstring -> Bigstring.set

let pp : type k wr async. ((t, wr, async) c, k) access -> k fmt = function
  | Bytes -> Bytes.pp
  | String -> String.pp
  | Bigstring -> Bigstring.pp

let unsafe_sub_pp : type k wr async.
    ((t, wr, async) c, k) access -> off:int -> len:int -> k fmt = function
  | Bytes -> Bytes.unsafe_sub_pp
  | String -> String.unsafe_sub_pp
  | Bigstring -> Bigstring.unsafe_sub_pp

let sub_pp : type k wr async.
    ((t, wr, async) c, k) access -> off:int -> len:int -> k fmt = function
  | Bytes -> Bytes.sub_pp
  | String -> String.sub_pp
  | Bigstring -> Bigstring.sub_pp

let compare : type k wr async. ((t, wr, async) c, k) access -> k compare =
  function
  | Bytes -> Bytes.compare
  | String -> String.compare
  | Bigstring -> Bigstring.compare

let unsafe_sub_compare : type k wr async.
    ((t, wr, async) c, k) access -> a:slice -> b:slice -> k compare = function
  | Bytes -> Bytes.unsafe_sub_compare
  | String -> String.unsafe_sub_compare
  | Bigstring -> Bigstring.unsafe_sub_compare

let sub_compare : type k wr async.
    ((t, wr, async) c, k) access -> a:slice -> b:slice -> k compare = function
  | Bytes -> Bytes.sub_compare
  | String -> String.sub_compare
  | Bigstring -> Bigstring.sub_compare

let equal : type k wr async. ((t, wr, async) c, k) access -> k equal = function
  | Bytes -> Bytes.equal
  | String -> String.equal
  | Bigstring -> Bigstring.equal

let unsafe_sub_equal : type k wr async.
    ((t, wr, async) c, k) access -> a:slice -> b:slice -> k equal = function
  | Bytes -> Bytes.unsafe_sub_equal
  | String -> String.unsafe_sub_equal
  | Bigstring -> Bigstring.unsafe_sub_equal

let sub_equal : type k wr async.
    ((t, wr, async) c, k) access -> a:slice -> b:slice -> k equal = function
  | Bytes -> Bytes.sub_equal
  | String -> String.sub_equal
  | Bigstring -> Bigstring.sub_equal

module Access = struct
  type ('a, 'k) t = ('a, 'k) access

  let equal : type a0 a1 k0 k1.
      (a0, k0) t -> (a1, k1) t -> (k0, k1) Refl.t option =
   fun a b ->
    match (a, b) with
    | Bytes, Bytes -> Some Refl.Refl
    | String, String -> Some Refl.Refl
    | Bigstring, Bigstring -> Some Refl.Refl
    | _a, _b -> None

  let read : type k rd wr async.
      ((rd, wr, async) c, k) t -> (rd, [`True]) Refl.t option = function
    | Bytes -> Some Refl.Refl
    | String -> Some Refl.Refl
    | Bigstring -> Some Refl.Refl

  let write : type k rd wr async.
      ((rd, wr, async) c, k) t -> (wr, [`True]) Refl.t option = function
    | Bytes -> Some Refl.Refl
    | Bigstring -> Some Refl.Refl
    | String -> None

  let async : type k rd wr async.
      ((rd, wr, async) c, k) t -> (async, [`True]) Refl.t option = function
    | Bigstring -> Some Refl.Refl
    | Bytes -> None
    | String -> None
end

let coerce : type a k. (a, k) access -> k tag = function
  | Bytes -> Buffet1.bytes
  | String -> Buffet1.string
  | Bigstring -> Buffet1.bigstring
