type base = ..

type error =
  [ `Not_found of Luma__id.Id.Resource.t
  | `Type_mismatch of Luma__id.Id.Resource.t
  ]

val error_to_string :
  [< `Not_found of Luma__id.Id.Resource.t | `Type_mismatch of Luma__id.Id.Resource.t ] -> string

module type S = sig
  type t

  val id : Luma__id.Id.Resource.t
  val name : string
  val pp : t Fmt.t
  val of_base : base -> t
  val of_base_opt : base -> t option
  val to_base : t -> base
end

module Make (B : sig
  type inner

  val name : string
end) : S with type t = B.inner

type packed = Packed : (module S with type t = 'a) * 'a -> packed

val pack : 'a. (module S with type t = 'a) -> 'a -> packed
val unpack : 'a. (module S with type t = 'a) -> packed -> ('a, error) result
val id : packed -> Luma__id.Id.Resource.t
val pp_packed : Format.formatter -> packed -> unit
