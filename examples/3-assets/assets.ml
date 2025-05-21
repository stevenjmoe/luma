module Driver = Luma__driver_raylib.Driver
module Luma = Luma.Make (Driver)
open Luma
module Velocity = [%component: Raylib.Vector2.t]
module Player_tag = [%component: int]
module Player_sounds = [%component: Luma.Sound.t Luma.Assets.handle]

[%%component
module Animation_config = struct
  type t = {
    first_index : int;
    last_index : int;
    frame_duration : float;
    mutable current_index : int;
    mutable frame_time_accumulator : float;
  }
end]

let render () =
  System.make_with_resources
    ~components:
      Query.Component.(Required (module Sprite.C) & Required (module Animation_config.C) & End)
    ~resources:Query.Resource.(Resource (module Assets.R) & End)
    "render"
    (fun world entities (assets, _) ->
      entities
      |> List.iter (fun (_, (sprite, (anim_config, _))) ->
             match Assets.get (module Luma.Image.Texture.A) assets (Sprite.image sprite) with
             | Some texture ->
                 let frame_index = Animation_config.(anim_config.current_index) in
                 Luma.Renderer.draw_texture texture ~position:(Math.Vec2.create 10. 10.)
                   ~size:(Math.Vec2.create 500. 500.) ~frame_index
                   ~texture_atlas:(Sprite.texture_atlas sprite) ()
                 |> ignore;
                 ()
             | None -> ());
      world)

let execute_animations () =
  System.make_with_resources
    ~components:Query.Component.(Required (module Animation_config.C) & End)
    ~resources:Query.Resource.(Resource (module Assets.R) & Resource (module Time.R) & End)
    "execute_animations"
    (fun world entities (assets, (time, _)) ->
      let dt = Time.dt time in
      entities
      |> List.iter (fun (_, (animation_config, _)) ->
             let open Animation_config in
             animation_config.frame_time_accumulator <-
               animation_config.frame_time_accumulator +. dt;

             if animation_config.frame_time_accumulator >= animation_config.frame_duration then (
               animation_config.frame_time_accumulator <-
                 animation_config.frame_time_accumulator -. animation_config.frame_duration;

               animation_config.current_index <-
                 (animation_config.current_index + 1) mod animation_config.last_index);
             ());
      world)

let input_system () =
  System.make_with_resources
    ~components:Query.Component.(Required (module Player_sounds.C) & End)
    ~resources:Query.Resource.(Resource (module Assets.R) & End)
    "input_system"
    (fun world entities (assets, _) ->
      entities
      |> List.iter (fun (_, (sounds, _)) ->
             let sound = Assets.get (module Luma.Sound.A) assets sounds |> Option.get in
             if Luma.Input.Keyboard.is_key_down @@ Luma.Key.Space then
               Luma.Sound.play_sound sound;
             ());
      world)

let setup_player () =
  System.make_with_resources ~components:End
    ~resources:Query.Resource.(Resource (module Asset_server.R) & Resource (module Assets.R) & End)
    "setup_player"
    (fun world entities (asset_server, (assets, _)) ->
      match Asset_server.load asset_server "examples/3-assets/assets/Player Idle 48x48.png" with
      | Ok handle ->
          let layout =
            Luma.Image.Texture_atlas_layout.from_grid (Luma.Math.Vec2.create 48. 48.) 10 1
          in
          let atlas = Luma.Image.Texture_atlas.from_layout layout in
          let sprite = Sprite.from_atlas_image handle atlas in
          let player_tag = 1 in
          let animation_config =
            Animation_config.
              {
                first_index = 1;
                last_index = 10;
                current_index = 1;
                frame_duration = 0.1;
                frame_time_accumulator = 0.0;
              }
          in
          (* Add the texture atlas to the global assets store *)
          Assets.add (module Luma.Image.Texture_atlas.A) assets atlas |> ignore;

          world
          |> World.add_entity
          |> World.with_component world (module Sprite.C) sprite
          |> World.with_component world (module Player_tag.C) player_tag
          |> World.with_component world (module Animation_config.C) animation_config
          |> ignore;

          world
      | Error _ -> failwith "Failed to load texture.")

let setup_player () =
  System.make_with_resources ~components:End
    ~resources:Query.Resource.(Resource (module Asset_server.R) & Resource (module Assets.R) & End)
    "setup_player"
    (fun world entities (asset_server, (assets, _)) : World.t ->
      let texture =
        Asset_server.load asset_server "examples/3-assets/assets/Player Idle 48x48.png"
        |> Result.get_ok
      in
      let sound =
        Asset_server.load asset_server
          "examples/3-assets/assets/FreeSFX/GameSFX/Animal Insects/Retro Birds 07.wav"
        |> Result.get_ok
      in
      let layout = Luma.Image.Texture_atlas_layout.from_grid (Luma.Math.Vec2.create 48. 48.) 10 1 in
      let atlas = Luma.Image.Texture_atlas.from_layout layout in
      let sprite = Sprite.from_atlas_image texture atlas in
      let player_tag = 1 in
      let animation_config =
        Animation_config.
          {
            first_index = 1;
            last_index = 10;
            current_index = 1;
            frame_duration = 0.1;
            frame_time_accumulator = 0.0;
          }
      in

      (* Add the texture atlas to the global assets store *)
      Assets.add (module Luma.Image.Texture_atlas.A) assets atlas |> ignore;

      world
      |> World.add_entity
      |> World.with_component world (module Sprite.C) sprite
      |> World.with_component world (module Player_tag.C) player_tag
      |> World.with_component world (module Animation_config.C) animation_config
      |> World.with_component world (module Player_sounds.C) sound
      |> ignore;
      world)

let () =
  let open Luma.App in
  create ()
  |> Plugin.add_default_plugins
  |> add_system (Startup (WithResources (setup_player ())))
  |> add_system (Render (WithResources (render ())))
  |> add_system (Update (WithResources (execute_animations ())))
  |> add_system (Update (WithResources (input_system ())))
  |> run
