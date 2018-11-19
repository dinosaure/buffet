open S
open Buffet1
open Buffet3

type t

val of_string : ?off:int -> ?len:int -> string -> t
val of_bytes : ?off:int -> ?len:int -> bytes -> t
val of_bigstring : ?off:int -> ?len:int -> bigstring -> t

val copy : (t, t) copy
(** [copy t] returns a new sequence that contains the same bytes as the
    argument. *)

val sub : (t, t) sub
(** [sub t ~off ~len] returns a new [t] of length [len], containing the
    subsequence of [t] that starts at position [off] and has length [len]. The
    new [t] inherits the same capabilities as [t]. If kind of [t] is
    {!Buffet2.bigstring}, new [t] is a proxy to a subsequence of [t] - we did
    not allocate properly and entirely a new [t].

    @raise [Invalid_argument] if [off] and [len] do not designate a valid range
    of [t].

    @raise [Invalid_argument] if [t] does not have a read-access. *)

val length : t length
(** Returns the length (number of bytes) of the argument. *)

val unsafe_get : (t, char) get
val unsafe_set : (t, char) set

val get : (t, char) get
(** [get t n] returns the byte at index [n] in argument [t].

    @raise [Invalid_argument] if [n] is not a valid index in [t].

    @raise [Invalid_argument] if [t] does not have a read-access. *)

val set : (t, char) set
(** [set t n c] modifies [t] in place, replacing the byte at index [n] with
    [c].

    @raise [Invalid_argument] if [n] is not a valid index in [t].

    @raise [Invalid_argument] if [t] does not have a write-access. *)

val pp : t fmt

val compare : t compare
(** The comparison function for [t], with the same specification as
    {!Pervasives.compare}. Along with the type [t], this function [compare]
    allows the module [Buffet4] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}.

    @raise [Invalid_argument] if [a] and [b] don't have the same kind.

    @raise [Invalid_argument] if [a] or [b] don't have a read-access. *)

val equal : t equal
(** The equality function for [t].

    @raise [Invalid_argument] if [a] and [b] don't have the same kind.

    @raise [Invalid_argument] if [a] or [b] don't have a read-access. *)
