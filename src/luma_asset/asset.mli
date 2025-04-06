type base = ..

module type S = sig
  type t

  val type_id : Luma__id.Id.Asset_type.t
  val of_base : base -> t
  val of_base_opt : base -> t option
  val to_base : t -> base
end

module Make (B : sig
  type inner
end) : S with type t = B.inner

type error = [ `Not_found of Luma__id.Id.Asset_type.t | `Type_mismatch of Luma__id.Id.Asset_type.t ]
type packed = Packed : (module S with type t = 'a) * 'a -> packed

val pack : 'a. (module S with type t = 'a) -> 'a -> packed
val unpack : 'a. (module S with type t = 'a) -> packed -> ('a, error) result
val type_id : packed -> Luma__id.Id.Asset_type.t
