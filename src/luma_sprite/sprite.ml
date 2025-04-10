type t = {
  mutable image : Luma__image.Image.Texture.t Luma__asset.Assets.handle;
  mutable texture_atlas : Luma__image.Image.Texture_atlas.t option;
  flip_x : bool;
  flip_y : bool;
  custom_size : Luma__math.Vec2.t option;
}

let image t = t.image
let texture_atlas t = t.texture_atlas
let set_image t image = t.image <- image
let set_texture_atlas t atlas = t.texture_atlas <- Some atlas

let sized image custom_size =
  { image; texture_atlas = None; flip_x = false; flip_y = false; custom_size = Some custom_size }

let from_image image =
  { image; texture_atlas = None; flip_x = false; flip_y = false; custom_size = None }

let from_atlas_image image texture_atlas =
  { image; texture_atlas = Some texture_atlas; flip_x = false; flip_y = false; custom_size = None }

let frame_size sprite =
  match sprite.texture_atlas with
  | None -> None
  | Some atlas -> Luma__image.Image.Texture_atlas.frame_size atlas

module C = Luma__ecs.Component.Make (struct
  type inner = t
end)
