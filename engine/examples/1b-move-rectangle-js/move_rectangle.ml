module Driver = Luma__driver_js.Driver
module Luma = Luma.Make (Driver)
open Luma
module Component = Component
module Rectangle = [%component: Math.Rect.t]
module Player_tag = [%component: int]

(* Define a component with a custom pp expression which uses the underlying component pp expression. *)
[%%component
module Velocity = struct
  type t = Math.Vec2.t

  let pp fmt t = Fmt.pf fmt "%a x: %f | y: %f" C.pp t (Math.Vec2.x t) (Math.Vec2.y t)
end]

let input_system () =
  System.make_with_resources
    ?filter:(Some Query.Component.Filter.(With Player_tag.C.id))
    ~components:Query.Component.(Required (module Velocity.C) & End)
    ~resources:Query.Resource.(Resource (module Time.R) & End)
    "input_system"
    (fun world entities (time, _) ->
      let open Math in
      let open Luma.Input.Keyboard in
      entities
      |> List.iter (fun (_, (velocity, _)) ->
             let dt = Time.dt time in
             let vx =
               if is_key_down Key.A then Vec2.x velocity -. (10. *. dt)
               else if is_key_down Key.D then Vec2.x velocity +. (10. *. dt)
               else 0.
             in

             let vy =
               if is_key_down Key.W then Vec2.y velocity -. (10. *. dt)
               else if is_key_down Key.S then Vec2.y velocity +. (10. *. dt)
               else 0.
             in
             Vec2.set_x velocity vx;
             Vec2.set_y velocity vy;
             ());
      world)

let movement_system () =
  System.make_with_resources
    ~components:
      Query.Component.(
        Required (module Rectangle.C)
        & Required (module Velocity.C)
        & Required (module Camera.C)
        & End)
    ~resources:Query.Resource.(Resource (module Time.R) & End)
    "movement_system"
    (fun world entities (time, _) ->
      let open Math in
      let open Luma.Camera in
      entities
      |> List.iter (fun (_, (rect, (velocity, (camera, _)))) ->
             Rect.set_x rect (Rect.x rect +. Vec2.x velocity);
             Rect.set_y rect (Rect.y rect +. Vec2.y velocity);
             Luma.Camera.set_target camera (Vec2.create (Rect.x rect) (Rect.y rect));
             ());
      world)

let render_system () =
  System.make
    ~components:Query.Component.(Required (module Rectangle.C) & End)
    "render_system"
    (fun world entities ->
      let open Raylib in
      entities
      |> List.iter (fun (_, (rectangle, _)) ->
             Renderer.draw_rect rectangle Colour.white;
             ());
      world)

[%%component
module Animation_config = struct
  type t = {
    first_index : int;
    last_index : int;
    frame_duration : float;
    mutable frame_time_accumulator : float;
  }
end]

let setup_rectangle () =
  System.make_with_resources
    ~components:Query.(End)
    ~resources:Query.Resource.(Resource (module Asset_server.R) & Resource (module Assets.R) & End)
    "setup_rectangle"
    (fun world entities res ->
      Query.Tuple.with2 res (fun asset_server assets ->
          let open World in
          let open Math in
          let player_tag = 1 in
          let rect = Rect.create ~pos:(Vec2.create 100. 50.) ~size:(Vec2.create 20. 50.) in
          let velocity = Math.Vec2.zero in
          let offset =
            Vec2.create
              (Float.of_int (Luma.screen_width ()) /. 2.)
              (Float.of_int (Luma.screen_height ()) /. 2.)
          in
          let target = Vec2.create (Rect.x rect) (Rect.y rect) in
          let camera = Camera.make ~offset ~target ~rotation:0. ~zoom:1. () in
          Luma.Camera.set_zoom camera 1.;

          let texture =
            Asset_server.load
              (module Image.Texture.A)
              asset_server "examples/3-assets/assets/Player Idle 48x48.png" world
            |> Result.get_ok
          in
          let layout =
            Luma.Image.Texture_atlas_layout.from_grid (Luma.Math.Vec2.create 48. 48.) 10 1
          in
          let atlas = Luma.Image.Texture_atlas.from_layout layout in
          let sprite = Sprite.from_atlas_image texture atlas in

          let position = Math.Vec3.create 60. 60. 200. in
          let scale = Math.Vec3.create 600. 600. 0. in
          let transform = Transform.create ~position ~scale () in
          let animation_config =
            Animation_config.
              {
                first_index = 1;
                last_index = 10;
                frame_duration = 0.1;
                frame_time_accumulator = 0.0;
              }
          in

          world
          |> add_entity
          |> with_component world (module Sprite.C) sprite
          |> with_component world (module Transform.C) transform
          |> with_component world (module Animation_config.C) animation_config
          |> ignore;
          world
          |> add_entity
          |> with_component world (module Player_tag.C) player_tag
          |> with_component world (module Rectangle.C) rect
          |> with_component world (module Velocity.C) velocity
          |> with_component world (module Camera.C) camera
          |> ignore;
          world))

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

let setup_other_rectangle () =
  System.make
    ~components:Query.(End)
    "setup_other_rectangle"
    (fun world entities ->
      let open World in
      let open Math in
      let rect = Rect.create ~pos:(Vec2.create 100. 50.) ~size:(Vec2.create 20. 50.) in

      world |> add_entity |> with_component world (module Rectangle.C) rect |> ignore;
      world)

let () =
  let window_config =
    Luma.Window_config.create 1920 1080 (Some (Colour.rgb ~r:100 ~g:299 ~b:200)) None false
  in
  let config = Luma.Plugin.Config.{ window = window_config; camera = Camera_config.default () } in
  App.create ()
  |> Luma.Plugin.add_default_plugins ~config
  |> App.on Startup (setup_rectangle ())
  |> App.on Scheduler.Startup (setup_other_rectangle ())
  |> App.on Scheduler.Update (input_system ())
  |> App.on Scheduler.Update (movement_system ())
  |> App.on Scheduler.Update (execute_animations ())
  |> App.on Scheduler.Render (render_system ())
  |> App.run
