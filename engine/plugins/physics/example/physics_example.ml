module Luma = Luma.Make (Luma_driver_raylib.Driver)
module Physics_plugin = Luma_physics.Physics.Make (Luma)
open Luma
open Luma_physics

module Player_ref = struct
  type t = Id.Entity.t

  module R = Luma__resource.Resource.Make (struct
    type inner = t

    let name = "player_ref"
  end)
end

let make_floor w cmd () =
  let floor_pos = Math.Vec2.create 0. 150. in
  let floor_size = Math.Vec2.create 500. 20. in
  let floor_center =
    Math.Vec2.create (floor_pos.x +. (floor_size.x *. 0.5)) (floor_pos.y +. (floor_size.y *. 0.5))
  in
  let floor_rb = Rigid_body.create_box Static floor_center floor_size in

  Command.spawn cmd [ Component ((module Rigid_body.C), floor_rb) ] |> ignore;
  w

let make_rect w ~pos ~size ~colour cmd () =
  let open Math.Vec2 in
  let rect1_center = Math.Vec2.create (pos.x +. (size.x *. 0.5)) (pos.y +. (size.y *. 0.5)) in
  let rb = Rigid_body.create_box Dynamic rect1_center size ~mass:1000. in

  Command.spawn cmd [ Component ((module Rigid_body.C), rb) ] |> ignore;
  w

let setup_rigid_bodies () =
  System.make ~components:End "setup_movable_rect" (fun w cmd e ->
      let rect_w, rect_h = (50., 50.) in
      let pos = Math.Vec2.create 50. 50. in
      let size = Math.Vec2.create rect_w rect_h in
      let colour = Colour.rgb ~r:100 ~g:100 ~b:255 in

      let w = make_floor w cmd () in
      let w = make_rect w ~pos ~size ~colour cmd () in

      let pos2 = Math.Vec2.create 500. 50. in
      let size2 = Math.Vec2.create rect_w rect_h in
      ignore (make_rect w ~pos:pos2 ~size:size2 ~colour cmd ());

      let pos3 = Math.Vec2.create 500. 150. in
      let size3 = Math.Vec2.create rect_w rect_h in
      let rect3_center =
        Math.Vec2.create (pos3.x +. (size3.x *. 0.5)) (pos3.y +. (size3.y *. 0.5))
      in
      let rb3 = Rigid_body.create_box Kinematic rect3_center size3 ~mass:10. in

      let player = Command.spawn ~name:"player" cmd [ Component ((module Rigid_body.C), rb3) ] in

      let packed_player = Resource.pack (module Player_ref.R) player in
      World.add_resource Player_ref.R.type_id packed_player w |> ignore;

      let circle_pos1 = Math.Vec2.create 350. 50. in
      let circle = Math.Primitives.Circle.create 50. circle_pos1 in
      let circle_rb = Rigid_body.create_circle Dynamic circle_pos1 circle.radius ~mass:1. in

      Command.spawn cmd [ Component ((module Rigid_body.C), circle_rb) ] |> ignore;

      let circle_pos2 = Math.Vec2.create 460. 50. in
      let circle2 = Math.Primitives.Circle.create 50. circle_pos2 in
      let circle_rb2 = Rigid_body.create_circle Dynamic circle_pos2 circle2.radius ~mass:1. in

      Command.spawn cmd [ Component ((module Rigid_body.C), circle_rb2) ] |> ignore;
      w)

let move_rect () =
  System.make
    ~components:Query.Component.(Required (module Rigid_body.C) & End)
    "move_rect"
    (fun w cmd e ->
      Query.Tuple.iter_e1
        (fun e rb ->
          let open Rigid_body in
          if rb.body_type = Kinematic then
            let pos = Input.Mouse.get_mouse_position () in

            match rb.shape with
            | Aabb a ->
                rb.pos.x <- pos.x;
                rb.pos.y <- pos.y;
                ()
            | _ -> ())
        e;
      w)

let print_collision () =
  System.make_with_resources ~components:End
    ~resources:Query.Resource.(Resource (module Player_ref.R) & End)
    "print_collision"
    (fun w cmd e (r, _) ->
      Collision_event.iter_events_for_entity ~entity:r w (fun ~other phase ~flags ->
          Printf.printf "Player collision: %s %d %d \n%!"
            (Collision_event.phase_to_string phase)
            (Id.Entity.to_int r) (Id.Entity.to_int other));
      w)

let () =
  let physics_config =
    Luma_physics.Config.create ~gravity:(Math.Vec2.create 0. 10.) ~debug:true ()
  in
  App.create ()
  |> Plugin.add_default_plugins
  |> App.add_plugin Plugin.debug_plugin
  |> App.add_plugin (Physics_plugin.plugin ~world_config:physics_config)
  |> App.on Startup (setup_rigid_bodies ())
  |> App.on Update (move_rect ())
  |> App.on Update (print_collision ())
  |> App.run
