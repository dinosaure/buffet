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
