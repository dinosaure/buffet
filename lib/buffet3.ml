open S
open Buffet0
open Buffet2

let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

let invalid_bounds off len real_len =
  invalid_arg "Invalid bounds on buffer (len: %d): off:%d, len:%d" real_len off
    len

type ('a, 'k) meta = {off: int; len: int; t: ('a, 'k) access}
type ('k0, 'k1) copy = 'k0 -> 'k1

let commit :
       off:int
    -> len:int
    -> ('a, 'k) access
    -> 'k
    -> (('a, 'k) meta, ('a, 'k) access * 'k) result =
 fun ~off ~len a k ->
  if off < 0 || len < 0 || off > length a k - len then Error (a, k)
  else Ok {off; len; t= a}

let make : type a k. (a, k) access -> int -> char -> (a, k) meta * k =
 fun t len chr -> ({off= 0; len; t}, make t len chr)

let empty : type a k. (a, k) access -> (a, k) meta * k =
 fun a -> ({off= 0; len= 0; t= a}, empty a)

let copy : type k. ('a, k) meta -> (k, ('a, k) meta * k) copy =
 fun {off; len; t} k -> ({off= 0; len; t}, unsafe_copy t ~off ~len k)

let with_len len k t =
  if len < 0 || t.off > length t.t k - len then Error (t.t, k)
  else Ok {t with len}

let with_off off k t =
  if off < 0 || off > length t.t k - t.len then Error (t.t, k)
  else Ok {t with off}

let ( >>= ) x f = match x with Ok v -> f v | Error _ as err -> err

let sub : type k. ('a, k) meta -> (k, ('a, k) meta * k) sub =
 fun t k ~off ~len ->
  match
    (with_len len k t >>= with_off (t.off + off) k, Access.equal t.t bigstring)
  with
  | Error (a, k), _ -> invalid_bounds off len (length a k)
  | Ok t, None -> (t, k)
  | Ok {off; len; t= a}, Some Refl.Refl ->
      ({off= 0; len; t= a}, Bigstring.unsafe_sub k ~off ~len)

let get t k off =
  match with_len 1 k t >>= with_off (t.off + off) k with
  | Ok {t; off; _} -> get t k off
  | Error (a, k) -> invalid_bounds t.off t.len (length a k)

let set t k off =
  match with_len 1 k t >>= with_off (t.off + off) k with
  | Ok {t; off; _} -> set t k off
  | Error (a, k) -> invalid_bounds t.off t.len (length a k)

let unsafe_get t k off = unsafe_get t.t k (t.off + off)
let unsafe_set t k off = unsafe_set t.t k (t.off + off)
let length {len; _} _ = len
let pp {off; len; t} = unsafe_sub_pp t ~off ~len
let to_slice : type a k. (a, k) meta -> slice = fun {off; len; _} -> {off; len}

let compare : type k.
       a:((t, 'wr0, 'async0) c, k) meta
    -> b:((t, 'wr1, 'async1) c, k) meta
    -> k compare =
 fun ~a ~b ->
  unsafe_sub_compare ~a:(to_slice a) ~b:(to_slice b) (* b.t = *) a.t

let equal : type k.
       a:((t, 'wr0, 'async0) c, k) meta
    -> b:((t, 'wr1, 'async1) c, k) meta
    -> k equal =
 fun ~a ~b -> unsafe_sub_equal ~a:(to_slice a) ~b:(to_slice b) (* b.t = *) a.t

let coerce : ('a, 'k) meta -> ('a, 'k) access = fun {t; _} -> t
