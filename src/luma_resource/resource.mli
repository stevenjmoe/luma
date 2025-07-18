type base = ..

module type S = sig
  type t

  val type_id : Luma__id.Id.Resource.t
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
val unpack : 'a. (module S with type t = 'a) -> packed -> ('a, Luma__core.Error.error) result
val unpack_opt : 'a. (module S with type t = 'a) -> packed -> 'a option
val type_id : packed -> Luma__id.Id.Resource.t

val pp_packed : Format.formatter -> packed -> unit
(** Print a packed component using its own pretty-printer. *)

val show : packed -> string
(** [show packed] takes a packed resource and returns a formatted string based on the resource's
    pretty printer. *)
