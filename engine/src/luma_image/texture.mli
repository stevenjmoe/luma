open Luma__app

module type S = sig
  type t

  module A : Luma__asset.Asset.S with type t = t
  module Assets : Luma__asset.Assets.For with type t = Luma__asset.Assets.t and type asset = t

  val add_plugin : App.t -> App.t
end

module Make : functor (D : Luma__driver.Driver.S) -> S with type t = D.Texture.t
