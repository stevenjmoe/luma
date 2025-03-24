type base = ..
type error = [ `Not_found of Luma__id.Id.id | `Type_mismatch of Luma__id.Id.id ]

val error_to_string : [< `Not_found of int | `Type_mismatch of int ] -> string

module type S = sig
  type t

  val id : Luma__id.Id.id
  val of_base : base -> t
  val of_base_opt : base -> t option
  val to_base : t -> base
end

module Make
    (Id : Luma__id.Id.S)
    (B : sig
      type inner
    end) : S with type t = B.inner

module type Packed = sig
  type packed = Packed : (module S with type t = 'a) * 'a -> packed

  val pack : (module S with type t = 'a) -> 'a -> packed
  val unpack : (module S with type t = 'a) -> packed -> ('a, error) result
  val id : packed -> int
end

module Packed : Packed
