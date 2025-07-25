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

module Make (D : Luma__driver.Driver.S) : S with type texture = D.texture = struct
  type texture = D.Texture.t

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

  type texture = D.Texture.t
  type queue = Sprite.t List.t

  module R = Luma__resource.Resource.Make (struct
    type inner = queue

    let name = "sprite_render_queue"
  end)

  let order_sprites () =
    Luma__ecs.System.make_with_resources
      ~components:Query.Component.(Required (module Sprite.C) & Required (module Transform.C) & End)
      ~resources:Query.Resource.(Resource (module R) & End)
      "order_sprites"
      (fun w e (queue, _) ->
        let open Transform in
        let sorted =
          e
          |> List.sort (fun (_, (_, (t1, _))) (_, (_, (t2, _))) ->
                 compare t1.position.z t2.position.z)
          |> List.map (fun (_, (sprite, _)) -> sprite)
        in
        let packed = Resource.pack (module R) sorted in
        Luma__ecs.World.set_resource R.type_id packed w)

  let render_ordered_sprites () =
    let open Render in
    let open Luma__asset in
    Luma__ecs.System.make_with_resources
      ~components:Query.Component.(Required (module Sprite.C) & Required (module Transform.C) & End)
      ~resources:Query.Resource.(Resource (module Luma__asset.Assets.R) & End)
      "render_ordered_sprites"
      (fun (w : World.t) e (assets, _) ->
        e
        |> List.iter (fun (_, (sprite, (transform, _))) ->
               let position =
                 Luma__transform.Transform.(
                   Luma__math.Vec2.create transform.position.x transform.position.y)
               in
               let size =
                 Luma__transform.Transform.(
                   Luma__math.Vec2.create transform.scale.x transform.scale.y)
               in
               let texture_atlas = Sprite.texture_atlas sprite in

               match Assets.get (module Texture.A) assets (Sprite.image sprite) with
               | Some t -> Renderer.draw_texture t ~position ~size ~texture_atlas ()
               | None ->
                   ();
                   ());
        w)

  let add_plugin app =
    let packed = Resource.pack (module R) [] in
    World.add_resource R.type_id packed (App.world app) |> ignore;
    app |> App.on Update @@ order_sprites () |> App.on Render @@ render_ordered_sprites ()
end
