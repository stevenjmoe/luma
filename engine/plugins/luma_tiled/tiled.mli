module Make (L : Luma.S) : sig
  module Tilemap_asset : L.Asset.S with type t = Types.t
  module Tileset_asset : L.Asset.S with type t = Types.tileset

  module Tilemap : sig
    type t

    val add : L.World.t -> string -> Luma__math.Vec2.t -> float -> int -> t option
  end

  val plugin : L.App.t -> L.App.t
end
