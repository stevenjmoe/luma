module Driver = Luma_driver_raylib.Driver
module Luma = Luma.Make (Driver)
open Luma
module Velocity = [%component: Raylib.Vector2.t]
module Player_tag = [%component: int]

module Game_state = struct
  type t =
    | InGame
    | Menu
    | Paused

  module S = Luma.State.Make (struct
    type inner = t
  end)
end

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
    (fun world _ entities res ->
      Query.Tuple.with2 res (fun assets time ->
          let dt = Time.dt time in
          entities
          |> Query.Tuple.iter2 (fun animation_config sprite ->
                 let open Animation_config in
                 let open Texture_atlas in
                 let atlas = Sprite.texture_atlas sprite |> Option.get in
                 animation_config.frame_time_accumulator <-
                   animation_config.frame_time_accumulator +. dt;

                 if animation_config.frame_time_accumulator >= animation_config.frame_duration then
                   (animation_config.frame_time_accumulator <-
                      animation_config.frame_time_accumulator -. animation_config.frame_duration;

                    Texture_atlas.set_index atlas
                      ((Texture_atlas.index atlas + 1) mod animation_config.last_index))
                   |> ignore;
                 ()));
      world)

let input_system () =
  System.make_with_resources
    ~components:Query.Component.(End)
    ~resources:
      Query.Resource.(Resource (module Assets.R) & Resource (module Luma.State.State_res.R) & End)
    "input_system"
    (fun world _ entities (assets, (state, _)) ->
      match state with
      | { current = Some current_state; previous = _; next = _ } ->
          if Luma.Input.Keyboard.is_key_pressed @@ Luma.Key.Space then
            if State.is (module Game_state.S) Game_state.InGame current_state then
              State.queue_state (module Game_state.S) Game_state.Menu world |> ignore
            else State.queue_state (module Game_state.S) Game_state.InGame world |> ignore;
          world
      | _ -> world)

let setup_player () =
  System.make_with_resources ~components:End
    ~resources:Query.Resource.(Resource (module Asset_server.R) & Resource (module Assets.R) & End)
    "setup_player"
    (fun world _ entities (asset_server, (assets, _)) : World.t ->
      let texture =
        Asset_server.load
          (module Image.Texture.A)
          asset_server "examples/3-assets/assets/Player Idle 48x48.png" world
        |> Result.get_ok
      in
      let layout = Luma.Image.Texture_atlas_layout.from_grid (Luma.Math.Vec2.create 48. 48.) 10 1 in
      let atlas = Luma.Image.Texture_atlas.from_layout layout in
      let sprite = Sprite.from_atlas_image texture atlas in
      let player_tag = 1 in

      let position = Math.Vec3.create 60. 60. 200. in
      let scale = Math.Vec3.create 600. 600. 0. in
      let transform = Transform.create ~position ~scale () in

      let animation_config =
        Animation_config.
          { first_index = 1; last_index = 10; frame_duration = 0.1; frame_time_accumulator = 0.0 }
      in

      (* Add the texture atlas to the global assets store *)
      Assets.add (module Luma.Image.Texture_atlas.A) assets atlas |> ignore;

      world
      |> World.add_entity
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
  |> init_state (module Game_state.S) Game_state.Menu
  |> while_in (module Game_state.S) Game_state.InGame ~stage:Update ~system:(execute_animations ())
  |> on Startup (setup_player ())
  |> on Update (input_system ())
  |> run
