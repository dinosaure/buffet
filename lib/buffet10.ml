open Buffet1

type 'a rd = < rd: unit ; .. > as 'a
type 'a wr = < wr: unit ; .. > as 'a
type 'a async = < async: unit ; .. > as 'a
type 'a aligned = < aligned: unit ; .. > as 'a

(* / *)

type rd_only = < rd: unit >
type wr_only = < wr: unit >

(* / *)

type ('a, 'k) t = 'k tag

external ro : ('a rd, 'k) t -> (rd_only, 'k) t = "%identity"
external wo : ('a wr, 'k) t -> (wr_only, 'k) t = "%identity"

include Buffet1
