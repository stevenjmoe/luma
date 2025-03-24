type base = ..

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

module Packed : sig
  type packed = Packed : (module S with type t = 'a) * 'a -> packed

  val pack : 'a. (module S with type t = 'a) -> 'a -> packed
  val unpack : 'a. (module S with type t = 'a) -> packed -> 'a option
  val id : packed -> int
end
