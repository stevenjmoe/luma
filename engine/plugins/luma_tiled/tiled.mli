module Make (L : Luma.S) : sig
  module Tilemap_asset : L.Asset.S with type t = Types.t
  module Tileset_asset : L.Asset.S with type t = Types.tileset

  val plugin : L.App.t -> L.App.t
end
