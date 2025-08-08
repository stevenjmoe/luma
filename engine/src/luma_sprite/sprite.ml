open Luma__image
open Luma__serialize
open Luma__core

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
    mutable image : texture Luma__asset.Assets.handle;
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

  let frame_size sprite =
    match sprite.texture_atlas with None -> None | Some atlas -> Texture_atlas.frame_size atlas

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
  open Luma__render
  open Luma__asset
  open Luma__math
  open Luma__id

  type texture = D.Texture.t
  type queue = Id.Entity.t List.t

  module R = Luma__resource.Resource.Make (struct
    type inner = queue

    let name = "sprite_render_queue"
  end)

  let log = Luma__core.Log.sub_log "sprite"

  let order_sprites () =
    Luma__ecs.System.make_with_resources
      ~components:Query.Component.(Required (module Sprite.C) & Required (module Transform.C) & End)
      ~resources:Query.Resource.(Resource (module R) & End)
      "order_sprites"
      (fun w e (queue, _) ->
        let sorted =
          e
          |> List.sort (fun (_, (_, (t1, _))) (_, (_, (t2, _))) ->
                 compare t1.Transform.position.z t2.position.z)
          |> List.map (fun (e, (_, _)) -> e)
        in
        let packed = Resource.pack (module R) sorted in
        Luma__ecs.World.set_resource R.type_id packed w)

  let render_ordered_sprites () =
    Luma__ecs.System.make_with_resources
      ~components:Query.Component.(End)
      ~resources:Query.Resource.(Resource (module Luma__asset.Assets.R) & Resource (module R) & End)
      "render_ordered_sprites"
      (fun w _ res ->
        Query.Tuple.with2 res (fun assets queue ->
            queue
            |> List.iter (fun (e : Id.Entity.t) ->
                   match
                     ( World.get_component w (module Sprite.C) e,
                       World.get_component w (module Transform.C) e )
                   with
                   | Some sprite, Some transform -> (
                       let position =
                         Transform.(Vec2.create transform.position.x transform.position.y)
                       in
                       let size = Transform.(Vec2.create transform.scale.x transform.scale.y) in
                       let texture_atlas = Sprite.texture_atlas sprite in
                       let flip_x = Sprite.flip_x sprite in
                       let flip_y = Sprite.flip_y sprite in

                       match Assets.get (module Texture.A) assets (Sprite.image sprite) with
                       | Some t ->
                           Renderer.draw_texture t ~position ~size ~texture_atlas ~flip_x ~flip_y ()
                       | None -> ())
                   | _, _ -> ());
            w))

  module Sprite_serializer =
    Serialize.Make_serializer
      (Serialize.Json_format)
      (struct
        open Yojson

        type t = Sprite.t

        let vec2 (v : Vec2.t) = `Assoc [ ("x", `Float v.x); ("y", `Float v.y) ]

        let to_repr sprite =
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
    (*let packed_serializer = Luma__serialize.Serialize.pack_json (module Sprite_serializer) in
    App.register_component Sprite.C.name (module Sprite.C) [ packed_serializer ] app |> ignore;*)
    app |> App.on Update @@ order_sprites () |> App.on Render @@ render_ordered_sprites ()
end
