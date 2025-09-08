module Make (L : Luma.S) : sig
  type t
  type tilemap

  module R : L.Resource.S with type t = t

  val add : L.World.t -> string -> Luma__math.Vec2.t -> float -> int -> t -> tilemap option
  val loaded : L.World.t -> L.Assets.handle -> bool
  val plugin : L.App.t -> L.App.t
end
