open Luma__image
open Luma__asset
open Luma__math

module type S = sig
  type t

  val image : t -> Assets.handle
  val path : t -> string option
  val texture_atlas : t -> Texture_atlas.t option
  val set_image : t -> Assets.handle -> unit
  val set_texture_atlas : t -> Texture_atlas.t -> unit
  val sized : Assets.handle -> Luma__math__Vec2.t -> t
  val from_image : Assets.handle -> t
  val from_atlas_image : Assets.handle -> Texture_atlas.t -> t
  val flip_x : t -> bool
  val flip_y : t -> bool
  val custom_size : t -> Vec2.t option
  val set_flip_x : t -> bool -> unit
  val set_flip_y : t -> bool -> unit

  module C : Luma__ecs.Component.S with type t = t
end

type spec = {
  path : string;
  texture_atlas : Texture_atlas.t option;
  flip_x : bool;
  flip_y : bool;
  custom_size : Luma__math.Vec2.t option;
}

type t = {
  mutable image : Assets.handle;
  mutable texture_atlas : Texture_atlas.t option;
  mutable flip_x : bool;
  mutable flip_y : bool;
  custom_size : Luma__math.Vec2.t option;
}

let image t = t.image
let texture_atlas t = t.texture_atlas
let path t = t.image.path
let set_image t image = t.image <- image
let set_texture_atlas t atlas = t.texture_atlas <- Some atlas
let flip_x t = t.flip_x
let flip_y t = t.flip_y
let custom_size t = t.custom_size
let set_flip_x t f = t.flip_x <- f
let set_flip_y t f = t.flip_y <- f

let sized image custom_size =
  { image; texture_atlas = None; flip_x = false; flip_y = false; custom_size = Some custom_size }

let from_image image =
  { image; texture_atlas = None; flip_x = false; flip_y = false; custom_size = None }

let from_atlas_image image texture_atlas =
  { image; texture_atlas = Some texture_atlas; flip_x = false; flip_y = false; custom_size = None }

module C = Luma__ecs.Component.Make (struct
  type inner = t

  let name = "Sprite"
end)

module Spec_c = Luma__ecs.Component.Make (struct
  type inner = spec

  let name = "Sprite spec"
end)
