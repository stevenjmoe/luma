open Utils
open Luma_physics
open Luma__math
open Alcotest

let v = Vec2.create

let with_store f =
  let shape_store = Shape_store.create () in
  let store = Rb_store.create () in
  f ~store ~shape_store

let add_circle ~store ~shape_store ~center ~radius ~mass ?(is_sensor = false) () =
  let rb = Rigid_body.create_circle ~mass ~is_sensor Dynamic center radius in
  Rb_store.add store shape_store rb

let add_box ~store ~shape_store ~center ~size =
  let rb = Rigid_body.create_box Static center size in
  Rb_store.add store shape_store rb

let add_poly ~store ~shape_store ~center ~points =
  let rb = Rigid_body.create_polygon_exn Static center points in
  Rb_store.add store shape_store rb

let check_aabb_poly ~name ~aabb_center ~aabb_size ~poly_center ~poly_points ~expect =
  with_store (fun ~store ~shape_store ->
      let row_a = add_box ~store ~shape_store ~center:aabb_center ~size:aabb_size in
      let row_b = add_poly ~store ~shape_store ~center:poly_center ~points:poly_points in
      let got = Narrow_phase.check_collision store shape_store ~row_a ~row_b in
      check bool name expect got)

let test_aabb_poly_overlap () =
  check_aabb_poly ~name:"aabb vs convex polygon overlap" ~aabb_center:(v 0. 0.) ~aabb_size:(v 2. 2.)
    ~poly_center:(v 0.5 0.)
    ~poly_points:[| v (-0.5) (-0.5); v 0.5 (-0.5); v 0.5 0.5; v (-0.5) 0.5 |]
    ~expect:true

let test_aabb_poly_separated () =
  check_aabb_poly ~name:"aabb vs convex polygon separated" ~aabb_center:(v 0. 0.)
    ~aabb_size:(v 2. 2.) ~poly_center:(v 3. 0.)
    ~poly_points:[| v (-0.5) (-0.5); v 0.5 (-0.5); v 0.5 0.5; v (-0.5) 0.5 |]
    ~expect:false

let test_aabb_poly_touch_edge () =
  check_aabb_poly ~name:"aabb vs convex polygon touch edge" ~aabb_center:(v 0. 0.)
    ~aabb_size:(v 2. 2.) ~poly_center:(v 1.5 0.)
    ~poly_points:[| v (-0.5) (-0.5); v 0.5 (-0.5); v 0.5 0.5; v (-0.5) 0.5 |]
    ~expect:true

let test_aabb_poly_touch_corner () =
  check_aabb_poly ~name:"aabb vs convex polygon touch corner" ~aabb_center:(v 0. 0.)
    ~aabb_size:(v 2. 2.) ~poly_center:(v 1.5 1.5)
    ~poly_points:[| v (-0.5) (-0.5); v 0.5 (-0.5); v 0.5 0.5; v (-0.5) 0.5 |]
    ~expect:true

let test_aabb_poly_rotated () =
  with_store (fun ~store ~shape_store ->
      let row_a = add_box ~store ~shape_store ~center:(v 0. 0.) ~size:(v 2. 2.) in
      let row_b =
        add_poly ~store ~shape_store ~center:(v 1.25 0.)
          ~points:[| v (-0.7) 0.; v 0. (-0.4); v 0.7 0.; v 0. 0.4 |]
      in
      Rb_store.set_angle store row_b (Float.pi /. 4.);
      let got = Narrow_phase.check_collision store shape_store ~row_a ~row_b in
      check bool "aabb vs rotated polygon overlap" true got)

let test_create_polygon_result_error_for_concave () =
  let concave = [| v 0. 0.; v 2. 0.; v 1. 0.5; v 2. 1.; v 0. 1. |] in
  match Rigid_body.create_polygon Static (v 0. 0.) concave with
  | Error Rigid_body.Non_convex_polygon -> ()
  | Ok _ -> fail "expected Error Non_convex_polygon"
  | Error _ -> fail "expected Error Non_convex_polygon"

let test_create_polygon_result_error_for_too_few_points () =
  match Rigid_body.create_polygon Static (v 0. 0.) [| v 0. 0.; v 1. 0. |] with
  | Error Rigid_body.Needs_at_least_3_points -> ()
  | Ok _ -> fail "expected Error Needs_at_least_3_points"
  | Error _ -> fail "expected Error Needs_at_least_3_points"

let test_create_polygon_exn_raises_for_concave () =
  let concave = [| v 0. 0.; v 2. 0.; v 1. 0.5; v 2. 1.; v 0. 1. |] in
  check_raises "create_polygon_exn concave"
    (Invalid_argument "Rigid_body.create_polygon_exn: polygon must be convex") (fun () ->
      ignore (Rigid_body.create_polygon_exn Static (v 0. 0.) concave))

let test_shape_store_add_polygon_rejects_concave () =
  let concave = [| v 0. 0.; v 2. 0.; v 1. 0.5; v 2. 1.; v 0. 1. |] in
  let shape_store = Shape_store.create () in
  check_raises "shape_store concave"
    (Invalid_argument "Shape_store.add_polygon: polygon must be convex") (fun () ->
      ignore (Shape_store.add shape_store (Rigid_body.Polygon concave)))

let test_sensor_pair_skips_resolution () =
  with_store (fun ~store ~shape_store ->
      let row_a = add_circle ~store ~shape_store ~center:(v 0. 0.) ~radius:1. ~mass:1. () in
      let row_b =
        add_circle ~is_sensor:true ~store ~shape_store ~center:(v 1.5 0.) ~radius:1. ~mass:1. ()
      in
      let np = Narrow_phase.create () in
      Dynarray.add_last np.ids1 row_a;
      Dynarray.add_last np.ids2 row_b;
      Rb_store.set_vel_x store row_a 2.;
      Rb_store.set_vel_x store row_b (-2.);

      Resolver.resolve_collisions ~restitution:1. ~position_correction:1. store shape_store np;

      check (float 1e-9) "sensor keeps vel a" 2. (Rb_store.vel_x store row_a);
      check (float 1e-9) "sensor keeps vel b" (-2.) (Rb_store.vel_x store row_b))

let test_sensor_event_flags_start_and_stop () =
  let entity_a = Luma__id.Id.Entity.of_int 1 in
  let entity_b = Luma__id.Id.Entity.of_int 2 in
  let pair_key = Narrow_phase.pair_key_of_pairs ~entity_a ~entity_b in
  let np = Narrow_phase.create () in
  let events = Collision_event.Collision_events_store.create () in

  Hashtbl.replace np.curr_pairs pair_key ();
  Hashtbl.replace np.curr_sensor_pairs pair_key ();
  Collision_event.fill_collision_events np events;

  let world = Luma__ecs.World.create () in
  let packed =
    Luma__resource.Resource.pack (module Collision_event.Collision_events_store.R) events
  in
  Luma__ecs.World.add_resource Collision_event.Collision_events_store.R.type_id packed world
  |> ignore;

  let start_count = ref 0 in
  Collision_event.iter_events world (fun event ->
      if Collision_event.is_removed event then ()
      else (
        incr start_count;
        check bool "start is sensor" true (Collision_event.is_sensor event);
        check bool "start not removed" false (Collision_event.is_removed event)));
  check int "one active event" 1 !start_count;

  Narrow_phase.clear np;
  Collision_event.fill_collision_events np events;

  let stop_count = ref 0 in
  Collision_event.iter_events world (fun event ->
      if Collision_event.is_removed event then (
        incr stop_count;
        check bool "stop is sensor" true (Collision_event.is_sensor event);
        check bool "stop removed" true (Collision_event.is_removed event)));
  check int "one stop event" 1 !stop_count

let tests =
  ( "collision",
    [
      "aabb_poly_overlap" -: test_aabb_poly_overlap;
      "aabb_poly_separated" -: test_aabb_poly_separated;
      "aabb_poly_touch_edge" -: test_aabb_poly_touch_edge;
      "aabb_poly_touch_corner" -: test_aabb_poly_touch_corner;
      "aabb_poly_rotated" -: test_aabb_poly_rotated;
      "create_polygon_result_concave" -: test_create_polygon_result_error_for_concave;
      "create_polygon_result_too_few_points" -: test_create_polygon_result_error_for_too_few_points;
      "create_polygon_exn_concave" -: test_create_polygon_exn_raises_for_concave;
      "shape_store_add_polygon_concave" -: test_shape_store_add_polygon_rejects_concave;
      "sensor_pair_skips_resolution" -: test_sensor_pair_skips_resolution;
      "sensor_event_flags_start_stop" -: test_sensor_event_flags_start_and_stop;
    ] )
