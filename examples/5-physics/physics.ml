module Driver = Luma__driver_raylib.Driver
module Luma = Luma.Make (Driver)
open Luma
module B = Box2d
module Rectangle = [%component: Math.Rect.t]
module Player_tag = [%component: int]
module Physics_body = [%component: B.Body_id.t]

(* pixels per meter *)
let ppm = 32.
let gravity_m_s2 = 10.
let run_speed_px_s = 256.
let run_speed_m_s = run_speed_px_s /. ppm
let time_step = 1. /. 60.
let sub_step_count = 4

let screen_to_box2d x_px y_px =
  let x_m = x_px /. ppm in
  let y_m = y_px /. ppm in
  B.Vec2.create x_m y_m

let spf float message = Printf.sprintf "Float print: %f. %s" float message

let spv vec message =
  Printf.sprintf "Vector print.\n x: %f. y: %f\n %s" (Math.Vec2.x vec) (Math.Vec2.y vec) message

let db message = Log.debug (fun l -> l "%s" message)

[%%component
module Velocity = struct
  type t = Math.Vec2.t

  let pp fmt t = Fmt.pf fmt "%a x: %f | y: %f" C.pp t (Math.Vec2.x t) (Math.Vec2.y t)
end]

module Physics_world = struct
  type t = B.World_id.t

  module R = Resource.Make (struct
    let name = "Physics_world"

    type inner = t
  end)
end

let input_system () =
  System.make
    ?filter:(Some Query.Component.Filter.(With Player_tag.C.id))
    ~components:Query.Component.(Required (module Velocity.C) & End)
    "input_system"
    (fun world entities ->
      let open Math in
      let open Luma.Input.Keyboard in
      let dx = (if is_key_down Key.D then 1. else 0.) -. if is_key_down Key.A then 1. else 0. in
      entities
      |> List.iter (fun (_, (velocity, _)) ->
             Math.Vec2.set_x velocity (dx *. run_speed_px_s);
             ());
      world)

let movement_system () =
  System.make
    ~components:
      Query.Component.(Required (module Velocity.C) & Required (module Physics_body.C) & End)
    "movement_system"
    (fun world entities ->
      let open Math in
      let open Luma.Camera.Component in
      entities
      |> List.iter (fun (_, (velocity, (pb, _))) ->
             let vx_m = Math.Vec2.x velocity /. ppm in
             let vy_m = B.Body.get_linear_velocity pb |> B.Vec2.y in
             B.Body.set_linear_velocity pb (B.Vec2.create vx_m vy_m);
             ());
      world)

let sync_system () =
  System.make
    ~components:
      Query.Component.(Required (module Physics_body.C) & Required (module Rectangle.C) & End)
    "sync_body_to_rect"
    (fun world entities ->
      entities
      |> List.iter (fun (_, (pb, (rect, _))) ->
             let pos_m = B.Body.get_position pb in
             let x_px = (B.Vec2.x pos_m *. ppm) -. (Math.Rect.width rect /. 2.) in
             let y_px = (B.Vec2.y pos_m *. ppm) -. (Math.Rect.height rect /. 2.) in
             Math.Rect.set_x rect (Float.round x_px);
             Math.Rect.set_y rect (Float.round y_px);
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
             Renderer.draw_rect rectangle @@ Colour.rgb ~r:100 ~g:150 ~b:255;
             ());
      world)

let physics_system () =
  System.make_with_resources
    ~components:
      Query.Component.(Required (module Physics_body.C) & Required (module Rectangle.C) & End)
    ~resources:Query.Resource.(Resource (module Physics_world.R) & End)
    "physics_system"
    (fun world entities (pw, _) ->
      B.World.step pw time_step sub_step_count;

      world)

let render_system () =
  System.make_with_resources
    ~components:
      Query.Component.(Required (module Rectangle.C) & Optional (module Physics_body.C) & End)
    ~resources:Query.Resource.(Resource (module Physics_world.R) & End)
    "render_system"
    (fun world entities (pw, _) ->
      entities
      |> List.iter (fun (_, (rect, (pb, _))) ->
             match pb with
             | Some p ->
                 let colour = Colour.rgb ~r:150 ~g:50 ~b:255 in
                 let rect = match pb with None -> rect | Some v -> rect in
                 Renderer.draw_rect rect colour
             | None ->
                 let colour = Colour.rgb ~r:100 ~g:150 ~b:255 in
                 let rect = match pb with None -> rect | Some v -> rect in
                 Renderer.draw_rect rect colour);

      world)

let update_camera_system () =
  System.make
    ~components:
      Query.Component.(Required (module Camera.Component.C) & Required (module Rectangle.C) & End)
    "update_camera"
    (fun world entities ->
      let open Camera.Component in
      let open Math in
      entities
      |> List.iter (fun (_, (c, (rect, _))) ->
             let cx = Math.Rect.x rect +. (Math.Rect.width rect /. 2.) in
             let cy = Math.Rect.y rect +. (Math.Rect.height rect /. 2.) in
             Luma.Camera.set_target c.camera (cx, cy);
             ());
      world)

let setup_physics_world () =
  System.make ~components:End "setup_physics_world" (fun world _ ->
      let w = B.World_def.default () in
      B.World_def.set_gravity w (B.Vec2.create 0. gravity_m_s2);
      let w = w |> Ctypes.addr |> B.World.create in
      let packed_physics = Resource.pack (module Physics_world.R) w in
      World.add_resource Physics_world.R.type_id packed_physics world |> ignore;
      world)

let setup_ground () =
  System.make_with_resources
    ~components:Query.(End)
    ~resources:Query.Resource.(Resource (module Physics_world.R) & End)
    "setup_other_rectangle"
    (fun world entities (pw, _) ->
      let open World in
      let open Math in
      (* Rectangle just for reference *)
      let rect = Rect.create ~pos:(Vec2.create 100. 150.) ~size:(Vec2.create 20. 50.) in

      let ground = Rect.create ~pos:(Vec2.create (-640.) 600.) ~size:(Vec2.create 1280. 720.) in
      let gx_px = Rect.x ground +. (Rect.width ground /. 2.) in
      let gy_px = Rect.y ground +. (Rect.height ground /. 2.) in

      let ground_body =
        B.Body_def.create ~position:(screen_to_box2d gx_px gy_px) ()
        |> Ctypes.addr
        |> B.Body.create pw
      in

      let ground_box =
        B.make_box (Rect.width ground /. 2. /. ppm) (Rect.height ground /. 2. /. ppm)
      in
      let ground_shape_def = B.Shape_def.default () in

      B.Shape.create_polygon ground_body (Ctypes.addr ground_shape_def) (Ctypes.addr ground_box)
      |> ignore;
      world |> add_entity |> with_component world (module Rectangle.C) ground |> ignore;
      world |> add_entity |> with_component world (module Rectangle.C) rect |> ignore;
      world)

let setup_rectangle () =
  System.make_with_resources
    ~components:Query.(End)
    ~resources:Query.Resource.(Resource (module Physics_world.R) & End)
    "setup_rectangle"
    (fun world entities (physics_world, _) ->
      let open World in
      let open Luma.Camera in
      let open Math in
      let player_tag = 1 in
      let rect = Rect.create ~pos:(Vec2.create 0. 0.) ~size:(Vec2.create 30. 50.) in
      let velocity = Math.Vec2.zero in
      let position =
        Vec2.create
          (Float.of_int (Luma.screen_width ()) /. 2.)
          (Float.of_int (Luma.screen_height ()) /. 2.)
      in
      let target = Vec2.create (Rect.x rect) (Rect.y rect) in
      let camera = Camera.make ~position ~target ~rotation:0. ~zoom:1. () in

      let body =
        B.Body_def.create
          ~position:(B.Vec2.create (Rect.width rect /. 2. /. ppm) (Rect.height rect /. 2. /. ppm))
          ~type_:B.Body_type.Dynamic ~fixed_rotation:true ()
        |> Ctypes.addr
        |> B.Body.create physics_world
      in

      let half_w_m = Rect.width rect /. 2. /. ppm in
      let half_h_m = Rect.height rect /. 2. /. ppm in

      let box = B.make_box half_w_m half_h_m in
      let shape = B.Shape_def.default () in
      B.Shape_def.set_density shape 1.;
      B.Surface_material.set_friction (B.Shape_def.material shape) 0.3;
      B.Shape.create_polygon body (Ctypes.addr shape) (Ctypes.addr box) |> ignore;

      world
      |> add_entity
      |> with_component world (module Player_tag.C) player_tag
      |> with_component world (module Rectangle.C) rect
      |> with_component world (module Velocity.C) velocity
      |> with_component world (module Camera.Component.C) { camera; active = true }
      |> with_component world (module Physics_body.C) body
      |> ignore;

      world)

let cleanup_physics_system () =
  System.make_with_resources
    ~components:Luma.Query.Component.(End)
    ~resources:Luma.Query.Resource.(Resource (module Physics_world.R) & End)
    "cleanup"
    (fun world entities (pw, _) ->
      Log.info (fun l -> l "cleaning up");
      Box2d.World.destroy pw;
      world)

let () =
  let open Luma.App in
  let window_config =
    Luma.Window_config.create 1280 720 (Some (Colour.rgb ~r:200 ~g:255 ~b:200)) None
  in
  let config = Luma.Plugin.Config.{ window = window_config } in

  create ()
  |> Plugin.add_default_plugins ~config
  |> add_system (Startup (WithoutResources (setup_physics_world ())))
  |> add_system (Startup (WithResources (setup_rectangle ())))
  |> add_system (Startup (WithResources (setup_ground ())))
  |> add_system (Update (WithoutResources (input_system ())))
  |> add_system (Update (WithoutResources (movement_system ())))
  |> add_system (Update (WithResources (physics_system ())))
  |> add_system (Update (WithoutResources (sync_system ())))
  |> add_system (Update (WithoutResources (update_camera_system ())))
  |> add_system (Render (WithResources (render_system ())))
  |> add_system (Cleanup (WithResources (cleanup_physics_system ())))
  |> run
