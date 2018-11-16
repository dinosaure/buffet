type ('p, 'f) app

module type S = sig
  type 'a s
  type t

  external inj : 'a s -> ('a, t) app = "%identity"
  external prj : ('a, t) app -> 'a s = "%identity"
end

module Make (T : sig
  type 'a t
end) : S with type 'a s = 'a T.t
