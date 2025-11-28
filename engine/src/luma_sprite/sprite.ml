open Luma__image
open Luma__serialize
open Luma__core
open Luma__math

module type S = sig
  type texture
  type t

  val image : t -> Luma__asset__Assets.handle
  val texture_atlas : t -> Texture_atlas.t option
  val set_image : t -> Luma__asset__Assets.handle -> unit
  val set_texture_atlas : t -> Texture_atlas.t -> unit
  val sized : Luma__asset__Assets.handle -> Luma__math__Vec2.t -> t
  val from_image : Luma__asset__Assets.handle -> t
  val from_atlas_image : Luma__asset__Assets.handle -> Texture_atlas.t -> t
  val flip_x : t -> bool
  val flip_y : t -> bool
  val custom_size : t -> Luma__math__Vec2.t option
  val set_flip_x : t -> bool -> unit
  val set_flip_y : t -> bool -> unit

  module C : Luma__ecs.Component.S with type t = t
end

module Make (D : Luma__driver.Driver.S) : S with type texture = D.texture = struct
  type texture = D.Texture.t

  type t = {
    mutable image : Luma__asset.Assets.handle;
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
  open Luma__app

  type texture
  type queue

  module R : Luma__resource.Resource.S

  val add_plugin : App.t -> App.t
end

module Sprite_plugin
    (D : Luma__driver.Driver.S)
    (Texture : Texture.S with type t = D.Texture.t)
    (Renderer : Luma__render.Render.Renderer with type texture = D.Texture.t)
    (Sprite : S with type texture = D.Texture.t) : Sprite_plugin with type texture = D.Texture.t =
struct
  open Luma__ecs
  open Luma__transform
  open Luma__resource
  open Luma__ecs
  open Luma__app
  open Luma__asset
  open Luma__math
  open Luma__id
  open Luma__render

  type texture = D.Texture.t
  type queue = Id.Entity.t List.t

  module R = Luma__resource.Resource.Make (struct
    type inner = queue

    let name = "sprite_render_queue"
  end)

  let log = Luma__core.Log.sub_log "sprite"

  let extract_sprites () =
    System.make_with_resources
      ~components:Query.Component.(Required (module Sprite.C) & Required (module Transform.C) & End)
      ~resources:
        Query.Resource.(Resource (module Assets.R) & Resource (module Renderer.Queue.R) & End)
      "extract_sprites"
      (fun world _ entities (assets, (queue, _)) ->
        Query.Tuple.iter2
          (fun sprite transform ->
            match Assets.get (module Texture.A) assets (Sprite.image sprite) with
            | None -> ()
            | Some tex ->
                let open Transform in
                let texture_atlas = Sprite.texture_atlas sprite in
                let texture_width = D.Texture.width tex |> float
                and texture_height = D.Texture.height tex |> float in

                let src =
                  match Sprite.texture_atlas sprite with
                  | Some ta -> Texture_atlas.get_frame ta
                  | None -> None
                in

                let size =
                  match src with
                  | Some r -> Vec2.create (Rect.width r) (Rect.height r)
                  | None -> Vec2.create texture_width texture_height
                in

                let flip_x = Sprite.flip_x sprite in
                let flip_y = Sprite.flip_y sprite in
                let z = int_of_float transform.position.z in
                let position = Vec2.create transform.position.x transform.position.y in
                let rotation = transform.rotation in

                Renderer.push_texture ~z ~tex ~position ~size ?texture_atlas ~flip_x ~flip_y ?src
                  ~rotation queue ())
          entities;
        world)

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
    let packed = Resource.pack (module R) [] in
    World.add_resource R.type_id packed (App.world app) |> ignore;

    let packed_serializer = Luma__serialize.Serialize.pack_json (module Sprite_serializer) in
    App.register_component Sprite.C.name (module Sprite.C) [ packed_serializer ] app |> ignore;

    app |> App.on PreRender @@ extract_sprites ()
end
