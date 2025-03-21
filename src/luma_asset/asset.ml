module Resource = Luma__resource.Resource
module Id = Luma__id.Id

type base = ..

module type S = sig
  type t

  val id : Id.Asset.t
  val of_base : base -> t
  val of_base_opt : base -> t option
  val to_base : t -> base
end

module Make_asset (B : sig
  type asset
end) : S with type t = B.asset = struct
  include B

  type t = asset
  type base += T of t

  let id = Id.Asset.next ()
  let of_base = function T t -> t | _ -> failwith ""
  let of_base_opt = function T t -> Some t | _ -> None
  let to_base t = T t
end

type asset = Asset : (module S with type t = 'a) * 'a -> asset
