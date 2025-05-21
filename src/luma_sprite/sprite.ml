open Luma__image

module type S = sig
  type texture
  type t

  val image : t -> texture Luma__asset__Assets.handle
  val texture_atlas : t -> Texture_atlas.t option
  val set_image : t -> texture Luma__asset__Assets.handle -> unit
  val set_texture_atlas : t -> Texture_atlas.t -> unit
  val sized : texture Luma__asset__Assets.handle -> Luma__math__Vec2.t -> t
  val from_image : texture Luma__asset__Assets.handle -> t
  val from_atlas_image : texture Luma__asset__Assets.handle -> Texture_atlas.t -> t
  val frame_size : t -> Luma__math__Vec2.t option

  module C : Luma__ecs.Component.S with type t = t
end

module Make (Texture : Texture.S) : S with type texture = Texture.t = struct
  type texture = Texture.t

  type t = {
    mutable image : texture Luma__asset.Assets.handle;
    mutable texture_atlas : Texture_atlas.t option;
    custom_size : Luma__math.Vec2.t option;
  }

  let image t = t.image
  let texture_atlas t = t.texture_atlas
  let set_image t image = t.image <- image
  let set_texture_atlas t atlas = t.texture_atlas <- Some atlas
  let sized image custom_size = { image; texture_atlas = None; custom_size = Some custom_size }
  let from_image image = { image; texture_atlas = None; custom_size = None }

  let from_atlas_image image texture_atlas =
    { image; texture_atlas = Some texture_atlas; custom_size = None }

  let frame_size sprite =
    match sprite.texture_atlas with None -> None | Some atlas -> Texture_atlas.frame_size atlas

  module C = Luma__ecs.Component.Make (struct
    type inner = t

    let name = "Sprite"
  end)
end
