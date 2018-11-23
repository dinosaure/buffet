(** Buffet0.

    The goal of this module is to provide the same interface between {!Bytes.t}
    and {!Bigstring.t} - and then provide a subset on {!String.t}. End-user is
    able to use this interface as a parameter for a {i functor} to be able to
    implement algorithm with a buffer abstraction.

    {[
      module type RDWR = sig
        type t

        include S.S0 with type t := t
        include S.S1 with type t := t
      end

      module Make (Buffer : RDWR) = struct .. end
    ]}

    About optimization, compiler is not able to optimize anything inside a
    {i functor}. *)

module Bytes : sig
  type t = bytes

  include S.S0 with type t := bytes
  include S.S1 with type t := bytes
end

module String : sig
  type t = string

  include S.S0 with type t := string
end

module Bigstring : sig
  type t =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  include S.S0 with type t := t
  include S.S1 with type t := t
end
