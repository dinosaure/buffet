open S
open Buffet2

type ('a, 'k) meta
 and ('k0, 'k1) copy = 'k0 -> 'k1

val commit :
     off:int
  -> len:int
  -> ('a, 'k) access
  -> 'k
  -> (('a, 'k) meta, ('a, 'k) access * 'k) result

val make : ('a, 'k) access -> int -> char -> ('a, 'k) meta * 'k
val empty : ('a, 'k) access -> ('a, 'k) meta * 'k

val copy :
     ((Bool.t, 'wr, 'async) c, 'k) meta
  -> ('k, ((Bool.t, 'wr, 'async) c, 'k) meta * 'k) copy

val with_len :
  int -> 'k -> ('a, 'k) meta -> (('a, 'k) meta, ('a, 'k) access * 'k) result

val with_off :
  int -> 'k -> ('a, 'k) meta -> (('a, 'k) meta, ('a, 'k) access * 'k) result

val ( >>= ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
val length : ('a, 'k) meta -> 'k length

val sub :
     ((Bool.t, 'wr, 'async) c, 'k) meta
  -> ('k, ((Bool.t, 'wr, 'async) c, 'k) meta * 'k) sub

val unsafe_get : ((Bool.t, 'wr, 'async) c, 'k) meta -> ('k, char) get
val unsafe_set : (('rd, Bool.t, 'async) c, 'k) meta -> ('k, char) set
val get : ((Bool.t, 'wr, 'async) c, 'k) meta -> ('k, char) get
val set : (('rd, Bool.t, 'async) c, 'k) meta -> ('k, char) set
val pp : ((Bool.t, 'wr, 'async) c, 'k) meta -> 'k fmt

val compare :
     a:((Bool.t, 'wr0, 'async0) c, 'k) meta
  -> b:((Bool.t, 'wr1, 'async1) c, 'k) meta
  -> 'k compare

val equal :
     a:((Bool.t, 'wr0, 'async0) c, 'k) meta
  -> b:((Bool.t, 'wr1, 'async1) c, 'k) meta
  -> 'k equal

val coerce : ('a, 'k) meta -> ('a, 'k) access
