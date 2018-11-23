open S
open Integer
open Buffet1

module Bool : sig
  type t = True
  type f = False
end

type ('rd, 'wr, 'async) c = < rd: 'rd ; wr: 'wr ; async: 'async >
type 'async ro = (Bool.t, Bool.f, 'async) c
type 'async wo = (Bool.f, Bool.t, 'async) c
type 'async rdwr = (Bool.t, Bool.t, 'async) c
type ('a, 'k) access

val bytes : (Bool.f rdwr, bytes) access
val string : (Bool.f ro, string) access
val bigstring : (Bool.t rdwr, bigstring) access

(** {2 Constructors} *)

val make : ('a, 'k) access -> int -> char -> 'k
(** [make witness n c] returns a fresh buffer of length [n], filled with the
    character [c].

    @raise [Invalid_argument] if [n < 0] or [n > {!Sys.max_string_length}]. *)

val empty : ('a, 'k) access -> 'k
(** [empty witness] is the empty buffer. It has length [0] and you can't really
    do much with it, but it's a good placeholder that only needs to be
    allocated once. *)

val copy : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, 'k) copy
(** [copy witness t ~off ~len] allocates a new buffer of length [len] and
    copies the bytes from [t] copied into it starting from [off]. *)

val sub : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, 'k) sub
(** [sub witness t ~off ~len] returns a new [t] of length [len], containing the
    subsequence of [t] that starts at position [off] and has length [len]. If
    [witness = {!bigstring}], [sub] does not allocate a bigstring, but instead
    returns a new view into [t] starting at [off], and with length [len].

    {b Note} If [witness = {!bigstring}] that this does not allocate a new
    buffer, but instead shares buffer of [t] with the newly-returned bigstring. *)

val create : (('rd, Bool.t, 'async) c, 'k) access -> int -> 'k
(** [create witness len] returns a buffer of length [len]. *)

(** {2 Memory-safe Operations} *)

val length : ('a, 'k) access -> 'k -> int
(** [length witness t] is the length of the buffer [t], in bytes. *)

val get : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, char) get
(** [get witness t n] returns the character at offset [n] in [t].

    @raise [Invalid_argument] if [n] is not a valid index in [t]. *)

val set : (('rd, Bool.t, 'async) c, 'k) access -> ('k, char) set
(** [set witness t n c] sets the character at offset [n] in [t] to be [c].

    @raise [Invalid_argument] if [n] is not a valid index in [t]. *)

(** {3 Little-endian Byte order} *)

val get_uint16_le :
  ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, [le | unsigned] int16) get
(** [get_uint16_le witness buf off] returns the two bytes as a little-endian
    unsigned 16-bit integer in [buf] starting at offset [off], interpreted as
    an unsigned integer.

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val get_int16_le : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, le int16) get
(** [get_int16_le witness buf off] returns the two bytes in [buf] as a
    little-endian signed 16-bit integer starting at offset [off], interpreted
    as a signed integer and performing sign extension to the native word size
    before returning the result.

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val set_int16_le : (('rd, Bool.t, 'async) c, 'k) access -> ('k, le int16) set
(** [set_int16_le witness buf off v] sets the two bytes in [buf] as a
    little-endian signed 16-bit integer starting at offset [off] to the value
    [v].

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val get_int32_le : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, le int32) get
(** [get_int32_le buf off] returns the four bytes as a little-endian signed
    32-bit integer in [buf] starting at offset [off].

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val set_int32_le : (('rd, Bool.t, 'async) c, 'k) access -> ('k, le int32) set
(** [set_int32_le witness buf off v] sets the four bytes in [buf] as a
    little-endian signed 32-bit integer starting at offset [off] to the value
    [v].

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val get_int64_le : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, le int64) get
(** [get_int64_le buf off] returns the eight bytes as a little-endian signed
    64-bit integer in [buf] starting at offset [off].

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val set_int64_le : (('rd, Bool.t, 'async) c, 'k) access -> ('k, le int64) set
(** [set_int64_le witness buf off v] sets the eight bytes in [buf] as a
    little-endian signed 64-bit integer starting at offset [off] to the value
    [v].

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

(** {3 Big-endian Byte order} *)

val get_uint16_be :
  ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, [be | unsigned] int16) get
(** [get_uint16_be witness buf off] returns the two bytes as a big-endian
    unsigned 16-bit integer in [buf] starting at offset [off], interpreted as
    an unsigned integer.

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val get_int16_be : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, be int16) get
(** [get_int16_be witness buf off] returns the two bytes in [buf] as a
    big-endian signed 16-bit integer starting at offset [off], interpreted as a
    signed integer and performing sign extension to the native word size before
    returning the result.

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val set_int16_be : (('rd, Bool.t, 'async) c, 'k) access -> ('k, be int16) set
(** [set_int16_be witness buf off v] sets the two bytes in [buf] as a
    big-endian signed 16-bit integer starting at offset [off] to the value [v].

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val get_int32_be : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, be int32) get
(** [get_int32_be buf off] returns the four bytes as a big-endian signed 32-bit
    integer in [buf] starting at offset [off].

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val set_int32_be : (('rd, Bool.t, 'async) c, 'k) access -> ('k, be int32) set
(** [set_int32_be witness buf off v] sets the four bytes in [buf] as a
    big-endian signed 32-bit integer starting at offset [off] to the value [v].

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val get_int64_be : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, be int64) get
(** [get_int64_be buf off] returns the eight bytes as a big-endian signed
    64-bit integer in [buf] starting at offset [off].

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val set_int64_be : (('rd, Bool.t, 'async) c, 'k) access -> ('k, be int64) set
(** [set_int64_be witness buf off v] sets the eight bytes in [buf] as a
    big-endian signed 64-bit integer starting at offset [off] to the value [v].

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

(** {2 Pretty-printers} *)

val pp : ((Bool.t, 'wr, 'async) c, 'k) access -> 'k fmt
(** Pretty-printer of [t]. *)

val sub_pp :
  ((Bool.t, 'wr, 'async) c, 'k) access -> off:int -> len:int -> 'k fmt
(** Pretty-printer of a subsequence of [t].

    @raise [Invalid_argument] if [off] and [len] do not designate a valid range
    of [t]. *)

(** {2 Equality and Order} *)

val compare : ((Bool.t, 'wr, 'async) c, 'k) access -> 'k compare
(** The comparison function for [t], with the same specification as
    {!Pervasives.compare}. *)

val sub_compare :
  ((Bool.t, 'wr, 'async) c, 'k) access -> a:slice -> b:slice -> 'k compare
(** [sub_compare ~a ~b x y] compares subsequence defined by [a] of [x] and
    subsequence defined by [b] on [y] with the same specification as
    {!Pervasives.compare}.

    @raise [Invalid_argument] if [a] or [b] do not designate a valid range
    respectively on [x] or [y]. *)

val equal : ((Bool.t, 'wr, 'async) c, 'k) access -> 'k equal
(** The equality function for [t]. *)

val sub_equal :
  ((Bool.t, 'wr, 'async) c, 'k) access -> a:slice -> b:slice -> 'k equal
(** [sub_equal ~a ~b x y] returns equality test on subsequence defined by [a]
    on [x] and subsequence defined by [b] on [y].

    @raise [Invalid_argument] if [a] or [b] do not designate a valid range
    respectively on [x] or [y]. *)

(** {2 Casts} *)

module Access : sig
  type ('a, 'k) t = ('a, 'k) access

  val equal : ('a0, 'k0) t -> ('a1, 'k1) t -> ('k0, 'k1) Refl.t option
  (** [equal w0 w1] tests if [w0] and [w1] have the same kind and propagates
      this assertion in the [Some {!Refl.Refl}] branch. *)

  val read : (('rd, 'wr, 'async) c, 'k) t -> ('rd, Bool.t) Refl.t option
  (** [read w] tests if [w] has the read capability and propagates this
      assertion in the [Some {!Refl.Refl}] branch. *)

  val write : (('rd, 'wr, 'async) c, 'k) t -> ('wr, Bool.t) Refl.t option
  (** [write w] tests if [w] has the write capability and propagates this
      assertion in the [Some {!Refl.Refl}] branch. *)

  val async : (('rd, 'wr, 'async) c, 'k) t -> ('async, Bool.t) Refl.t option
  (** [async w] tests if [w] has the async capability and propagates this
      assertion in the [Some {!Refl.Refl}] branch. *)
end

val coerce : ('a, 'k) access -> 'k tag
(** [coerce w] casts [w] to a {!Buffet1.tag} (and lost capabilities meta-data). *)

(** {2 Memory-unsafe Operations} *)

val unsafe_sub : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, 'k) sub
val unsafe_copy : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, 'k) copy
val unsafe_get : ((Bool.t, 'wr, 'async) c, 'k) access -> ('k, char) get
val unsafe_set : (('rd, Bool.t, 'async) c, 'k) access -> ('k, char) set

val unsafe_sub_pp :
  ((Bool.t, 'wr, 'async) c, 'k) access -> off:int -> len:int -> 'k fmt

val unsafe_sub_compare :
  ((Bool.t, 'wr, 'async) c, 'k) access -> a:slice -> b:slice -> 'k compare

val unsafe_sub_equal :
  ((Bool.t, 'wr, 'async) c, 'k) access -> a:slice -> b:slice -> 'k equal
