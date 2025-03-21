type base = ..

module type S = sig
  type t

  val id : Luma__id.Id.Asset.t
  val of_base : base -> t
  val of_base_opt : base -> t option
  val to_base : t -> base
end

type asset = Asset : (module S with type t = 'a) * 'a -> asset

module Make_asset (B : sig
  type asset
end) : S with type t = B.asset
