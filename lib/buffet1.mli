open S
open Integer
open Buffet0

type bytes = Bytes.t
type string = String.t
type bigstring = Bigstring.t
type 'k tag

val bytes : bytes tag
val string : string tag
val bigstring : bigstring tag

(** {2 Constructors} *)

val make : 'k tag -> int -> char -> 'k
(** [make witness n c] returns a fresh buffer of length [n], filled with the
    character [c].

    @raise [Invalid_argument] if [n < 0] or [n > {!Sys.max_string_length}]. *)

val empty : 'k tag -> 'k
(** [empty witness] is the empty buffer. It has length [0] and you can't really
    do much with it, but it's a good placeholder that only needs to be
    allocated once. *)

val copy : 'k tag -> ('k, 'k) copy
(** [copy witness t ~off ~len] allocates a new buffer of length [len] and
    copies the bytes from [t] copied into it starting from [off]. *)

val sub : 'k tag -> ('k, 'k) sub
(** [sub witness t ~off ~len] returns a new [t] of length [len], containing the
    subsequence of [t] that starts at position [off] and has length [len]. If
    [witness = {!bigstring}], [sub] does not allocate a bigstring, but instead
    returns a new view into [t] starting at [off], and with length [len].

    {b Note} If [witness = {!bigstring}] that this does not allocate a new
    buffer, but instead shares buffer of [t] with the newly-returned bigstring. *)

val create : 'k tag -> int -> 'k
(** [create witness len] returns a buffer of length [len].

    @raise [Invalid_argument] if [witness = {!string}]. *)

(** {2 Memory-safe Operations} *)

val length : 'k tag -> 'k length
(** [length witness t] is the length of the buffer [t], in bytes. *)

val get : 'k tag -> ('k, char) get
(** [get witness t n] returns the character at offset [n] in [t].

    @raise [Invalid_argument] if [n] is not a valid index in [t]. *)

val set : 'k tag -> ('k, char) set
(** [set witness t n c] sets the character at offset [n] in [t] to be [c].

    @raise [Invalid_argument] if [n] is not a valid index in [t].

    @raise [Invalid_argument] if [witness = {!string}]. *)

val unsafe_get_uint8 : 'k tag -> ('k, unsigned int8) get
  [@@specialize always] [@@inline always]

val get_uint8 : 'k tag -> ('k, unsigned int8) get
  [@@specialize always] [@@inline always]

val unsafe_get_int8 : 'k tag -> ('k, signed int8) get
  [@@specialize always] [@@inline always]

val get_int8 : 'k tag -> ('k, signed int8) get
  [@@specialize always] [@@inline always]

val unsafe_set_int8 : 'k tag -> ('k, signed int8) set
  [@@specialize always] [@@inline always]

val set_int8 : 'k tag -> ('k, signed int8) set
  [@@specialize always] [@@inline always]

(** {3 Little-endian Byte order} *)

val get_uint16_le : 'k tag -> ('k, [le | unsigned] int16) get
(** [get_uint16_le witness buf off] returns the two bytes as a little-endian
    unsigned 16-bit integer in [buf] starting at offset [off], interpreted as
    an unsigned integer.

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val get_int16_le : 'k tag -> ('k, le int16) get
(** [get_int16_le witness buf off] returns the two bytes in [buf] as a
    little-endian signed 16-bit integer starting at offset [off], interpreted
    as a signed integer and performing sign extension to the native word size
    before returning the result.

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val set_int16_le : 'k tag -> ('k, le int16) set
(** [set_int16_le witness buf off v] sets the two bytes in [buf] as a
    little-endian signed 16-bit integer starting at offset [off] to the value
    [v].

    @raise [Invalid_argument] if [off] is not a valid index in [buf].

    @raise [Invalid_argument] if [witness = {!string}]. *)

val get_int32_le : 'k tag -> ('k, le int32) get
(** [get_int32_le buf off] returns the four bytes as a little-endian signed
    32-bit integer in [buf] starting at offset [off].

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val set_int32_le : 'k tag -> ('k, le int32) set
(** [set_int32_le witness buf off v] sets the four bytes in [buf] as a
    little-endian signed 32-bit integer starting at offset [off] to the value
    [v].

    @raise [Invalid_argument] if [off] is not a valid index in [buf].

    @raise [Invalid_argument] if [witness = {!string}]. *)

val get_int64_le : 'k tag -> ('k, le int64) get
(** [get_int64_le buf off] returns the eight bytes as a little-endian signed
    64-bit integer in [buf] starting at offset [off].

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val set_int64_le : 'k tag -> ('k, le int64) set
(** [set_int64_le witness buf off v] sets the eight bytes in [buf] as a
    little-endian signed 64-bit integer starting at offset [off] to the value
    [v].

    @raise [Invalid_argument] if [off] is not a valid index in [buf].

    @raise [Invalid_argument] if [witness = {!string}]. *)

(** {3 Big-endian Byte order} *)

val get_uint16_be : 'k tag -> ('k, [be | unsigned] int16) get
(** [get_uint16_be witness buf off] returns the two bytes as a big-endian
    unsigned 16-bit integer in [buf] starting at offset [off], interpreted as
    an unsigned integer.

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val get_int16_be : 'k tag -> ('k, be int16) get
(** [get_int16_be witness buf off] returns the two bytes in [buf] as a
    big-endian signed 16-bit integer starting at offset [off], interpreted as a
    signed integer and performing sign extension to the native word size before
    returning the result.

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val set_int16_be : 'k tag -> ('k, be int16) set
(** [set_int16_be witness buf off v] sets the two bytes in [buf] as a
    big-endian signed 16-bit integer starting at offset [off] to the value [v].

    @raise [Invalid_argument] if [off] is not a valid index in [buf].

    @raise [Invalid_argument] if [witness = {!string}]. *)

val get_int32_be : 'k tag -> ('k, be int32) get
(** [get_int32_be buf off] returns the four bytes as a big-endian signed 32-bit
    integer in [buf] starting at offset [off].

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val set_int32_be : 'k tag -> ('k, be int32) set
(** [set_int32_be witness buf off v] sets the four bytes in [buf] as a
    big-endian signed 32-bit integer starting at offset [off] to the value [v].

    @raise [Invalid_argument] if [off] is not a valid index in [buf].

    @raise [Invalid_argument] if [witness = {!string}]. *)

val get_int64_be : 'k tag -> ('k, be int64) get
(** [get_int64_be buf off] returns the eight bytes as a big-endian signed
    64-bit integer in [buf] starting at offset [off].

    @raise [Invalid_argument] if [off] is not a valid index in [buf]. *)

val set_int64_be : 'k tag -> ('k, be int64) set
(** [set_int64_be witness buf off v] sets the eight bytes in [buf] as a
    big-endian signed 64-bit integer starting at offset [off] to the value [v].

    @raise [Invalid_argument] if [off] is not a valid index in [buf].

    @raise [Invalid_argument] if [witness = {!string}]. *)

(** {2 Pretty-printers} *)

val pp : 'k tag -> 'k fmt
(** Pretty-printer of [t]. *)

val sub_pp : 'k tag -> off:int -> len:int -> 'k fmt
(** Pretty-printer of a subsequence of [t].

    @raise [Invalid_argument] if [off] and [len] do not designate a valid range
    of [t]. *)

(** {2 Equality and Order} *)

val compare : 'k tag -> 'k compare
(** The comparison function for [t], with the same specification as
    {!Pervasives.compare}. *)

val sub_compare : 'k tag -> a:slice -> b:slice -> 'k compare
(** [sub_compare ~a ~b x y] compares subsequence defined by [a] of [x] and
    subsequence defined by [b] on [y] with the same specification as
    {!Pervasives.compare}.

    @raise [Invalid_argument] if [a] or [b] do not designate a valid range
    respectively on [x] or [y]. *)

val equal : 'k tag -> 'k equal
(** The equality function for [t]. *)

val sub_equal : 'k tag -> a:slice -> b:slice -> 'k equal
(** [sub_equal ~a ~b x y] returns equality test on subsequence defined by [a]
    on [x] and subsequence defined by [b] on [y].

    @raise [Invalid_argument] if [a] or [b] do not designate a valid range
    respectively on [x] or [y]. *)

(** {2 Casts} *)

type value = Bytes of bytes | String of string | Bigstring of bigstring

val to_value : 'k tag -> 'k -> value
(** [to_value witness buf] wraps [buf] as a {!value}. *)

type v = V : 'k tag * 'k -> v

val of_value : value -> v
(** [of_value v] packs a buffer [v] to a couple [(witness, buffer)]. *)

module Tag : sig
  type 'k t = 'k tag

  val equal : 'a t -> 'b t -> ('a, 'b) Refl.t option
  (** [equal w0 w1] tests if [w0] and [w1] have the same kind and propagates
      this assertion in the [Some {!Refl.Refl}] branch. *)
end

val coerce : value -> 'k tag -> ('k tag * 'k) option
(** [coerce v witness] unpacks a buffer [v] with the witness [witness]. If
    [witness] is correct, it returns the witness of the buffer and the buffer.
    Otherwise, it returns [None]. *)

(** {2 Memory-unsafe Operations} *)

val unsafe_copy : 'k tag -> ('k, 'k) copy
val unsafe_sub : 'k tag -> ('k, 'k) sub
val unsafe_get_uint16_le : 'k tag -> ('k, [le | unsigned] int16) get
val unsafe_get_int16_le : 'k tag -> ('k, le int16) get
val unsafe_get_int32_le : 'k tag -> ('k, le int32) get
val unsafe_get_int64_le : 'k tag -> ('k, le int64) get
val unsafe_get_uint16_be : 'k tag -> ('k, [be | unsigned] int16) get
val unsafe_get_int16_be : 'k tag -> ('k, be int16) get
val unsafe_get_int32_be : 'k tag -> ('k, be int32) get
val unsafe_get_int64_be : 'k tag -> ('k, be int64) get
val unsafe_set_int16_le : 'k tag -> ('k, le int16) set
val unsafe_set_int32_le : 'k tag -> ('k, le int32) set
val unsafe_set_int64_le : 'k tag -> ('k, le int64) set
val unsafe_set_int16_be : 'k tag -> ('k, be int16) set
val unsafe_set_int32_be : 'k tag -> ('k, be int32) set
val unsafe_set_int64_be : 'k tag -> ('k, be int64) set
val unsafe_sub_pp : 'k tag -> off:int -> len:int -> 'k fmt
val unsafe_sub_compare : 'k tag -> a:slice -> b:slice -> 'k compare
val unsafe_sub_equal : 'k tag -> a:slice -> b:slice -> 'k equal
val unsafe_get : 'k tag -> ('k, char) get
val unsafe_set : 'k tag -> ('k, char) set
