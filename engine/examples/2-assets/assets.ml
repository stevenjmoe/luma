module Driver = Luma_driver_raylib.Driver
module Luma = Luma.Make (Driver)
open Luma
module Player_tag = [%component: int]

[%%component
module Animation_config = struct
  type t = {
    first_index : int;
    last_index : int;
    frame_duration : float;
    mutable frame_time_accumulator : float;
  }
end]

let execute_animations () =
  System.make_with_resources
    ~components:
      Query.Component.(Required (module Animation_config.C) & Required (module Sprite.C) & End)
    ~resources:Query.Resource.(Resource (module Assets.R) & Resource (module Time.R) & End)
    "execute_animations"
    (fun world entities (assets, (time, _)) ->
      let dt = Time.dt time in
      entities
      |> Query.Tuple.iter2 (fun animation_config sprite ->
             let open Animation_config in
             let atlas = Sprite.texture_atlas sprite |> Option.get in
             animation_config.frame_time_accumulator <-
               animation_config.frame_time_accumulator +. dt;

             if animation_config.frame_time_accumulator >= animation_config.frame_duration then (
               animation_config.frame_time_accumulator <-
                 animation_config.frame_time_accumulator -. animation_config.frame_duration;

               Texture_atlas.set_index atlas
                 ((Texture_atlas.index atlas + 1) mod animation_config.last_index));
             ());
      world)

let setup_player () =
  System.make_with_resources ~components:End
    ~resources:Query.Resource.(Resource (module Asset_server.R) & Resource (module Assets.R) & End)
    "setup_player"
    (fun world entities (asset_server, (assets, _)) : World.t ->
      let texture =
        Asset_server.load
          (module Image.Texture.A)
          asset_server "examples/2-assets/assets/Player Idle 48x48.png" world
        |> Result.get_ok
      in
      let layout = Luma.Image.Texture_atlas_layout.from_grid (Luma.Math.Vec2.create 48. 48.) 10 1 in
      let atlas = Luma.Image.Texture_atlas.from_layout layout in
      let sprite = Sprite.from_atlas_image texture atlas in
      let player_tag = 1 in

      let position = Math.Vec3.create 60. 60. 200. in
      let scale = Math.Vec3.create 1. 1. 0. in
      let transform = Transform.create ~position ~scale () in

      let animation_config =
        Animation_config.
          { first_index = 1; last_index = 10; frame_duration = 0.1; frame_time_accumulator = 0.0 }
      in

      (* Add the texture atlas to the global assets store *)
      Assets.add (module Luma.Image.Texture_atlas.A) assets atlas |> ignore;

      world
      |> World.add_entity ~name:"player"
      |> World.with_component world (module Sprite.C) sprite
      |> World.with_component world (module Player_tag.C) player_tag
      |> World.with_component world (module Animation_config.C) animation_config
      |> World.with_component world (module Transform.C) transform
      |> ignore;

      world)

let () =
  let open Luma.App in
  create ()
  |> Plugin.add_default_plugins
  |> App.add_plugin Plugin.debug_plugin
  |> on Startup (setup_player ())
  |> on Update (execute_animations ())
  |> run
