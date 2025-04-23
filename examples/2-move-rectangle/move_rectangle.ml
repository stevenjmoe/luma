module Driver = Luma__driver_raylib.Driver
module Luma = Luma.Make (Driver)
open Luma
module Component = Component
module Rectangle = [%component: Raylib.Rectangle.t]
module Velocity = [%component: Raylib.Vector2.t]
module Player_tag = [%component: int]

let input_system () =
  System.make_with_resources
    ?filter:(Some Query.Filter.(With Player_tag.C.id))
    ~components:Query.(Required (module Velocity.C) & End)
    ~resources:Resource.Query.(Resource (module Time.R) & End)
    (fun world entities (time, _) ->
      let open Raylib in
      entities
      |> List.iter (fun (_, (velocity, _)) ->
             let dt = time.dt in
             let vx =
               if is_key_down Key.A then
                 Vector2.x velocity -. (10. *. dt)
               else if is_key_down Key.D then
                 Vector2.x velocity +. (10. *. dt)
               else
                 0.
             in

             let vy =
               if is_key_down Key.W then
                 Vector2.y velocity -. (10. *. dt)
               else if is_key_down Key.S then
                 Vector2.y velocity +. (10. *. dt)
               else
                 0.
             in
             Vector2.set_x velocity vx;
             Vector2.set_y velocity vy;
             ());
      world)

let movement_system () =
  System.make_with_resources
    ~components:
      Query.(
        Required (module Rectangle.C)
        & Required (module Velocity.C)
        & Required (module Camera.Component.C)
        & End)
    ~resources:Resource.Query.(Resource (module Time.R) & End)
    (fun world entities (time, _) ->
      let open Raylib in
      let open Luma.Camera.Component in
      entities
      |> List.iter (fun (_, (rect, (velocity, (camera, _)))) ->
             Rectangle.set_x rect (Rectangle.x rect +. Vector2.x velocity);
             Rectangle.set_y rect (Rectangle.y rect +. Vector2.y velocity);
             Luma.Camera.set_target camera.camera (Raylib.Rectangle.x rect, Raylib.Rectangle.y rect);
             ());
      world)

let render_system () =
  System.make
    ~components:Query.(Required (module Rectangle.C) & End)
    (fun world entities ->
      let open Raylib in
      entities
      |> List.iter (fun (_, (rectangle, _)) ->
             Raylib.draw_rectangle
               (Int.of_float (Rectangle.x rectangle))
               (Int.of_float (Rectangle.y rectangle))
               (Int.of_float (Rectangle.width rectangle))
               (Int.of_float (Rectangle.height rectangle))
               Color.pink;
             ());
      world)

let setup_rectangle () =
  System.make
    ~components:Query.(End)
    (fun world entities ->
      let open World in
      let open Luma.Camera in
      let player_tag = 1 in
      let rect = Raylib.Rectangle.create 100. (-10.) 100. 50. in
      let velocity = Raylib.Vector2.zero () in
      let position =
        ( Float.of_int (Raylib.get_screen_width ()) /. 2.,
          Float.of_int (Raylib.get_screen_height ()) /. 2. )
      in
      let target = (Raylib.Rectangle.x rect, Raylib.Rectangle.y rect) in
      let camera = Camera.make ~position ~target ~rotation:0. ~zoom:1. in

      world
      |> add_entity
      |> with_component world (module Player_tag.C) player_tag
      |> with_component world (module Rectangle.C) rect
      |> with_component world (module Velocity.C) velocity
      |> with_component world (module Camera.Component.C) { camera; active = true }
      |> ignore;
      world)

let setup_other_rectangle () =
  System.make
    ~components:Query.(End)
    (fun world entities ->
      let open World in
      let rect = Raylib.Rectangle.create 100. 50. 20. 50. in

      world |> add_entity |> with_component world (module Rectangle.C) rect |> ignore;
      world)

let () =
  let window_config = Luma.Window_config.create 10 10 (Some Colour.white) None in
  let config = Luma.Plugins.Config.{ window = window_config } in
  App.create ()
  |> Luma.Plugins.add_default_plugins ~config
  |> App.add_system (Scheduler.Startup (Luma.System.WithoutResources (setup_rectangle ())))
  |> App.add_system (Scheduler.Startup (System.WithoutResources (setup_other_rectangle ())))
  |> App.add_system (Scheduler.Update (Luma.System.WithResources (input_system ())))
  |> App.add_system (Scheduler.Update (Luma.System.WithResources (movement_system ())))
  |> App.add_system (Scheduler.Render (Luma.System.WithoutResources (render_system ())))
  |> App.run
