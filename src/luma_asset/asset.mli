type base = ..

module type S = sig
  type t

  val id : Luma__id.Id.Asset.t
  val of_base : base -> t
  val of_base_opt : base -> t option
  val to_base : t -> base
  val file_extensions : string list
  val decode : string -> t
end

module Make (B : sig
  type inner

  val file_extensions : string list
  val decode : string -> inner
end) : S with type t = B.inner

type error = [ `Not_found of Luma__id.Id.id | `Type_mismatch of Luma__id.Id.id ]
type packed = Packed : (module S with type t = 'a) * 'a -> packed

val pack : 'a. (module S with type t = 'a) -> 'a -> packed
val unpack : 'a. (module S with type t = 'a) -> packed -> ('a, error) result
val id : packed -> int
