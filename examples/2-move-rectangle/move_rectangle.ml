module Driver = Luma__driver_raylib.Driver
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
               if is_key_down Key.A then
                 Vec2.x velocity -. (10. *. dt)
               else if is_key_down Key.D then
                 Vec2.x velocity +. (10. *. dt)
               else
                 0.
             in

             let vy =
               if is_key_down Key.W then
                 Vec2.y velocity -. (10. *. dt)
               else if is_key_down Key.S then
                 Vec2.y velocity +. (10. *. dt)
               else
                 0.
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
        & Required (module Camera.Component.C)
        & End)
    ~resources:Query.Resource.(Resource (module Time.R) & End)
    "movement_system"
    (fun world entities (time, _) ->
      let open Math in
      let open Luma.Camera.Component in
      entities
      |> List.iter (fun (_, (rect, (velocity, (camera, _)))) ->
             Rect.set_x rect (Rect.x rect +. Vec2.x velocity);
             Rect.set_y rect (Rect.y rect +. Vec2.y velocity);
             Luma.Camera.set_target camera.camera (Rect.x rect, Rect.y rect);
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

let setup_rectangle () =
  System.make
    ~components:Query.(End)
    "setup_rectangle"
    (fun world entities ->
      let open World in
      let open Luma.Camera in
      let open Math in
      let player_tag = 1 in
      let rect = Rect.create ~pos:(Vec2.create 100. 50.) ~size:(Vec2.create 20. 50.) in
      let velocity = Math.Vec2.zero in
      let position =
        Vec2.create
          (Float.of_int (Luma.screen_width ()) /. 2.)
          (Float.of_int (Luma.screen_height ()) /. 2.)
      in
      let target = Vec2.create (Rect.x rect) (Rect.y rect) in
      let camera = Camera.make ~position ~target ~rotation:0. ~zoom:1. () in

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
    "setup_other_rectangle"
    (fun world entities ->
      let open World in
      let open Math in
      let rect = Rect.create ~pos:(Vec2.create 100. 50.) ~size:(Vec2.create 20. 50.) in

      world |> add_entity |> with_component world (module Rectangle.C) rect |> ignore;
      world)

let () =
  let window_config =
    Luma.Window_config.create 1080 1920 (Some (Colour.rgb ~r:100 ~g:299 ~b:200)) None
  in
  let config = Luma.Plugin.Config.{ window = window_config } in
  App.create ()
  |> Luma.Plugin.add_default_plugins ~config
  |> App.add_system (Scheduler.Startup (Luma.System.WithoutResources (setup_rectangle ())))
  |> App.add_system (Scheduler.Startup (System.WithoutResources (setup_other_rectangle ())))
  |> App.add_system (Scheduler.Update (Luma.System.WithResources (input_system ())))
  |> App.add_system (Scheduler.Update (Luma.System.WithResources (movement_system ())))
  |> App.add_system (Scheduler.Render (Luma.System.WithoutResources (render_system ())))
  |> App.run
