open Luma__image
open Luma__asset
open Luma__serialize
open Luma__core
open Luma__math

module type S = sig
  type t

  val image : t -> Assets.handle
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

module Make (D : Luma__driver.Driver.S) : S = struct
  type t = {
    mutable image : Assets.handle;
    mutable texture_atlas : Texture_atlas.t option;
    mutable flip_x : bool;
    mutable flip_y : bool;
    custom_size : Luma__math.Vec2.t option;
  }

  let image t = t.image
  let texture_atlas t = t.texture_atlas
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
    {
      image;
      texture_atlas = Some texture_atlas;
      flip_x = false;
      flip_y = false;
      custom_size = None;
    }

  module C = Luma__ecs.Component.Make (struct
    type inner = t

    let name = "Sprite"
  end)
end

module type Sprite_plugin = sig
  val add_plugin : Luma__app__App.t -> Luma__app__App.t
end

module Sprite_plugin (D : Luma__driver.Driver.S) (Sprite : S) : Sprite_plugin = struct
  open Luma__id

  let log = Luma__core.Log.sub_log "sprite"

  module Sprite_serializer =
    Serialize.Make_serializer
      (Serialize.Json_format)
      (struct
        open Yojson

        type t = Sprite.t

        let vec2 (v : Vec2.t) = `Assoc [ ("x", `Float v.x); ("y", `Float v.y) ]

        (* TODO: *)
        let image handle =
          let open Assets in
          match handle.path with Some path -> "" | None -> ""

        let to_repr sprite =
          let open Sprite in
          let image = ("image", `String "TODO") in
          let texture_atlas = ("texture_atlas", `String "TODO") in
          let flip_x = ("flip_x", `Bool (Sprite.flip_x sprite)) in
          let flip_y = ("flip_y", `Bool (Sprite.flip_y sprite)) in
          let custom_size =
            match Sprite.custom_size sprite with
            | None -> `Null
            | Some v -> `List [ `Float (Luma__math.Vec2.x v); `Float (Luma__math.Vec2.y v) ]
          in
          `Assoc
            [
              ( Sprite.C.name,
                `Assoc [ image; texture_atlas; flip_x; flip_y; ("custom_size", custom_size) ] );
            ]

        let of_repr = function `Assoc [] | _ -> Error (Error.parse_json (Json "TODO"))
      end)

  let add_plugin app =
    let open Luma__app in
    let packed_serializer = Luma__serialize.Serialize.pack_json (module Sprite_serializer) in
    App.register_component Sprite.C.name (module Sprite.C) [ packed_serializer ] app |> ignore;

    app
end
