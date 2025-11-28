module Driver = Luma_driver_raylib.Driver
module Luma = Luma.Make (Driver)
open Luma
module Component = Component
module Rectangle = [%component: Math.Rect.t]
module Player_tag = [%component: unit]

(* Define a component with a custom pp expression which uses the underlying component pp expression. *)
[%%component
module Velocity = struct
  type t = Math.Vec2.t

  let pp fmt t = Fmt.pf fmt "%a x: %f | y: %f" C.pp t (Math.Vec2.x t) (Math.Vec2.y t)
end]

let input_system () =
  System.make
    ?filter:(Some Query.Component.Filter.(With Player_tag.C.id))
    ~components:Query.Component.(Required (module Velocity.C) & End)
    "input_system"
    (fun world _ entities ->
      let open Math in
      let open Luma.Input.Keyboard in
      Query.Tuple.iter1
        (fun velocity ->
          let vx =
            if is_key_down Key.A then Vec2.x velocity -. 20.
            else if is_key_down Key.D then Vec2.x velocity +. 20.
            else 0.
          in

          let vy =
            if is_key_down Key.W then Vec2.y velocity -. 20.
            else if is_key_down Key.S then Vec2.y velocity +. 20.
            else 0.
          in
          Vec2.set_x velocity vx;
          Vec2.set_y velocity vy;
          ())
        entities;
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
    (fun world _ entities (time, _) ->
      let open Math in
      let open Luma.Camera in
      let dt = Time.dt time in
      Query.Tuple.iter3
        (fun rect velocity camera ->
          Rect.set_x rect (Rect.x rect +. (Vec2.x velocity *. dt));
          Rect.set_y rect (Rect.y rect +. (Vec2.y velocity *. dt));
          Luma.Camera.set_target camera (Vec2.create (Rect.x rect +. 20.) (Rect.y rect +. 20.));
          ())
        entities;
      world)

let setup_rectangle () =
  System.make
    ~components:Query.(End)
    "setup_rectangle"
    (fun world _ _ ->
      let open World in
      let open Math in
      let player_tag = () in
      let rect = Rect.create ~pos:(Vec2.create 100. 50.) ~size:(Vec2.create 20. 50.) in
      let velocity = Math.Vec2.zero in
      let offset =
        Vec2.create
          (Float.of_int (Luma.screen_width ()) /. 2.)
          (Float.of_int (Luma.screen_height ()) /. 2.)
      in
      let target = Vec2.create (Rect.x rect +. 20.) (Rect.y rect +. 20.) in
      let camera = Camera.make ~offset ~target ~rotation:0. ~zoom:1. () in

      World.add_entity_with_components world ~name:"player"
        [
          Component.pack (module Player_tag.C) player_tag;
          Component.pack (module Rectangle.C) rect;
          Component.pack (module Velocity.C) velocity;
          Component.pack (module Camera.C) camera;
        ]
      |> ignore;

      world)

let setup_other_rectangle () =
  System.make
    ~components:Query.(End)
    "setup_other_rectangle"
    (fun world _ entities ->
      let open World in
      let open Math in
      let rect = Rect.create ~pos:(Vec2.create 100. 50.) ~size:(Vec2.create 359. 100.) in

      world |> add_entity |> with_component world (module Rectangle.C) rect |> ignore;
      world)

let draw_rects () =
  System.make_with_resources
    ~components:Query.Component.(Required (module Rectangle.C) & End)
    ~resources:Query.Resource.(Resource (module Renderer.Queue.R) & End)
    "draw_rects"
    (fun world _ entities (queue, _) ->
      Query.Tuple.iter1
        (fun rect ->
          Renderer.Draw.rect ~rect ~colour:(Colour.rgb ~r:50 ~g:50 ~b:50) queue;
          ())
        entities;
      world)

let () =
  let window_config =
    Luma.Window_config.create 1920 1080 (Some (Colour.rgb ~r:100 ~g:299 ~b:200)) None false
  in
  let config =
    Luma.Plugin.Config.
      { window = window_config; camera = Luma.Camera_config.make ~default_camera:false }
  in
  App.create ()
  |> App.add_plugin Plugin.debug_plugin
  |> Luma.Plugin.add_default_plugins ~config
  |> App.on Startup (setup_rectangle ())
  |> App.on Scheduler.Startup (setup_other_rectangle ())
  |> App.on Scheduler.Update (input_system ())
  |> App.on Scheduler.Update (movement_system ())
  |> App.on Scheduler.PreRender (draw_rects ())
  |> App.run
