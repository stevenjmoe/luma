open Alcotest
open Luma__math

let ( -: ) n f = Alcotest.test_case n `Quick f
let e = 1e-6

let check_vec name a b =
  let open Vec2 in
  Alcotest.(check (float 1e-6)) (name ^ " x") b.x a.x;
  Alcotest.(check (float 1e-6)) (name ^ " y") b.y a.y

let check_float name expected actual = Alcotest.(check (float e)) name expected actual
let v x y = Vec2.create x y
let aabb_minmax x0 y0 x1 y1 = Bounded2d.Aabb2d.of_min_max (v x0 y0) (v x1 y1)
let aabb_center cX cY hx hy = Bounded2d.Aabb2d.of_center_halfsize (v cX cY) (v hx hy)
let circle cx cy r = Bounded2d.Bounding_circle.create (v cx cy) r

let check_opt_float name expected actual =
  match (expected, actual) with
  | None, None -> ()
  | Some e, Some a -> check (float e) name e a
  | _ -> fail (name ^ ": expected " ^ match expected with Some _ -> "Some" | None -> "None")

module Rotation = struct
  let test_creation () =
    let rotation1 = Rot2.half_pi in
    let rotation2 = Rot2.of_degrees 90. in
    let rotation3 = Rot2.of_sin_cos 1. 0. in
    let rotation4 = Rot2.turn_fraction 0.25 in

    check (float e) "rotation1.sin and rotation2.sin are equal" rotation1.sin rotation2.sin;
    check (float e) "rotation1.cos and rotation2.cos are equal" rotation1.cos rotation2.cos;
    check (float e) "rotation1.sin and rotation3.sin are equal" rotation1.sin rotation3.sin;
    check (float e) "rotation1.cos and rotation3.cos are equal" rotation1.cos rotation3.cos;
    check (float e) "rotation1.sin and rotation4.sin are equal" rotation1.sin rotation4.sin;
    check (float e) "rotation1.cos and rotation4.cos are equal" rotation1.cos rotation4.cos;
    ()

  let test_rotate () =
    let rotation = Rot2.of_degrees 90. in
    let x_axis = Vec2.create 1. 0. in
    let y_axis = Vec2.create 0. 1. in
    let neg_x = Vec2.create (-1.) 0. in

    check_vec "X->Y" (Rot2.rotate_vec rotation x_axis) y_axis;
    check_vec "Y->-X" (Rot2.rotate_vec rotation y_axis) neg_x;
    ()

  let test_normalise () =
    let rotation = Rot2.of_sin_cos 10. 5. in
    let normalised = Rot2.normalise rotation in

    check (float e) "" 0.89442724 normalised.sin;
    check (float e) "" 0.44721362 normalised.cos;
    ()
end

module Circle = struct
  let test_circle_closest_point () =
    let open Primitives.Circle in
    let circle = { radius = 2.0 } in
    let center = Vec2.zero in

    let point_in = Vec2.create 1.0 1.0 in
    check_vec "inside" (closest_point circle point_in center) point_in;

    let point_on = Vec2.create 2.0 0.0 in
    check_vec "on-boundary" (closest_point circle point_on center) point_on;

    let point_out_x = Vec2.create 3.0 0.0 in
    let expect_x = Vec2.create 2.0 0.0 in
    check_vec "outside axis" (closest_point circle point_out_x center) expect_x;

    let point_out_diag = Vec2.create 3.0 4.0 in
    let expect_diag = Vec2.create 1.2 1.6 in
    check_vec "outside diagonal" (closest_point circle point_out_diag center) expect_diag;

    let point_zero = Vec2.zero in
    check_vec "origin" (closest_point circle point_zero center) point_zero

  let test_circle_closest_point_offset_center () =
    let open Primitives.Circle in
    let circle = { radius = 2.0 } in
    let center = Vec2.create 10.0 (-5.0) in

    (* Inside: should return the point unchanged *)
    let p_inside = Vec2.create 11.0 (-4.0) in
    check_vec "inside offset" (closest_point circle p_inside center) p_inside;

    (* Outside on +X axis from center: should clamp to center + (r,0) *)
    let p_out_x = Vec2.create 20.0 (-5.0) in
    let expect_x = Vec2.create 12.0 (-5.0) in
    check_vec "outside axis offset" (closest_point circle p_out_x center) expect_x;

    (* Outside diagonal: direction (3,4) from center -> normalized (0.6,0.8) *)
    let p_out_diag = Vec2.create 13.0 (-1.0) in
    (* center + (3,4) *)
    let expect_diag = Vec2.create 11.2 (-3.4) in
    (* center + 2*(0.6,0.8) *)
    check_vec "outside diagonal offset" (closest_point circle p_out_diag center) expect_diag
end

module Vec2 = struct
  open Vec2

  let test_distance () =
    let a = create 0.0 0.0 in
    let b = create 3.0 4.0 in

    (* classic 3-4-5 triangle *)
    check_float "distance 3-4-5" 5.0 (distance a b);
    check_float "distance symmetric" (distance b a) (distance a b);

    (* identical points *)
    check_float "distance zero" 0.0 (distance a a);

    (* axis-aligned *)
    let c = create 5.0 0.0 in
    check_float "distance axis-x" 5.0 (distance a c);

    let d = create 0.0 (-7.0) in
    check_float "distance axis-y" 7.0 (distance a d)

  let test_distance_squared () =
    let a = create 0.0 0.0 in
    let b = create 3.0 4.0 in

    check_float "distance² 3-4-5" 25.0 (distance_squared a b);
    check_float "distance² symmetric" (distance_squared b a) (distance_squared a b);

    (* consistent with distance *)
    let expected = Float.pow (distance a b) 2.0 in
    check_float "distance² consistent" expected (distance_squared a b)
end

module Bounded2d_tests = struct
  open Bounded2d
  open Aabb2d
  open Luma__math

  let check_aabb name (a : Aabb2d.t) (b : Aabb2d.t) =
    check_vec (name ^ " min") (min a) (min b);
    check_vec (name ^ " max") (max a) (max b)

  let test_of_center_halfsize () =
    let c = Vec2.create 3. 4. in
    let hs = Vec2.create 2. 1. in
    let a = of_center_halfsize c hs in

    check_vec "center" (center a) c;
    check_vec "half_size" (half_size a) hs;
    check_vec "min" (min a) (Vec2.sub c hs);
    check_vec "max" (max a) (Vec2.add c hs);
    ()

  let test_of_min_max () =
    let mn = Vec2.create 1. 2. and mx = Vec2.create 5. 6. in
    let a = of_min_max mn mx in
    check_vec "min" (min a) mn;
    check_vec "max" (max a) mx;
    check_vec "center" (center a) (Vec2.create 3. 4.);
    check_vec "half_size" (half_size a) (Vec2.create 2. 2.);
    ()

  let test_visible_area () =
    let a = of_min_max Vec2.zero (Vec2.create 4. 3.) in

    check_float "area" 12. (Aabb2d.visible_area a);

    (* degenerate / inverted gives non-negative area via clamp-to-zero extents *)
    let bad = Aabb2d.of_min_max (Vec2.create 5. 7.) (Vec2.create 2. 3.) in
    check_float "area non-negative" 0. (Aabb2d.visible_area bad)

  let test_contains_merge () =
    let a = of_min_max Vec2.zero (Vec2.create 4. 3.) in
    let b = of_min_max (Vec2.create 1. 1.) (Vec2.create 2. 2.) in
    let c = of_min_max (Vec2.create (-1.) 1.) (Vec2.create 5. 2.) in

    (check bool) "a contains b" true (contains a b);
    (check bool) "a !contains c" false (contains a c);

    let u = merge a c in
    check_vec "merge min" (min u) (Vec2.create (-1.) 0.);
    check_vec "merge max" (max u) (Vec2.create 5. 3.)

  let test_grow_shrink () =
    let a = of_min_max Vec2.zero (Vec2.create 4. 3.) in
    let amt = Vec2.create 1. 2. in
    let g = grow a amt in

    check_vec "grow min" (min g) (Vec2.create (-1.) (-2.));
    check_vec "grow max" (max g) (Vec2.create 5. 5.);

    let s = shrink a amt in
    check_vec "shrink min" (min s) (Vec2.create 1. 2.);
    check_vec "shrink max" (max s) (Vec2.create 3. 1.)

  let test_bounding_circle () =
    (* AABB centered at origin with width=4, height=2 ⇒ diagonal=√20, r=√20/2 *)
    let a = of_min_max (Vec2.create (-2.) (-1.)) (Vec2.create 2. 1.) in
    let bc = bounding_circle a in

    check_vec "bc center" (Bounding_circle.center bc) Vec2.zero;
    check_float "bc radius" (0.5 *. Float.sqrt 20.) (Bounding_circle.radius bc)

  let test_circle_aabb_roundtrip_contains () =
    let c = Vec2.create 1. 2. and r = 3. in
    let circ = Bounding_circle.create c r in
    let a = Bounding_circle.aabb_2d circ in

    (* Circle’s AABB should be [center ± r] *)
    check_vec "aabb min" (min a) (Vec2.create (c.x -. r) (c.y -. r));
    check_vec "aabb max" (max a) (Vec2.create (c.x +. r) (c.y +. r));

    (* A rect’s circumscribed circle then reboxed AABB must contain the original rect *)
    let rect = of_min_max Vec2.zero (Vec2.create 4. 1.) in
    let rect_circle = bounding_circle rect in
    let rect_circle_aabb = Bounding_circle.aabb_2d rect_circle in
    check bool "reboxed contains original" true (contains rect_circle_aabb rect)

  let test_aabb_aabb_overlap () =
    let a = aabb_minmax 0. 0. 4. 3. in
    let b = aabb_minmax 2. 1. 6. 5. in
    check bool "overlap" true (Aabb2d.intersects_aabb a b);
    check bool "symmetry" true (Aabb2d.intersects_aabb b a)

  let test_aabb_aabb_touch_edge () =
    (* b touches a on the right edge at x=4 *)
    let a = aabb_minmax 0. 0. 4. 3. in
    let b = aabb_minmax 4. (-1.) 6. 1. in
    check bool "touch edge counts as intersect" true (Aabb2d.intersects_aabb a b)

  let test_aabb_aabb_touch_corner () =
    let a = aabb_minmax 0. 0. 2. 2. in
    let b = aabb_minmax 2. 2. 3. 3. in
    check bool "touch corner counts as intersect" true (Aabb2d.intersects_aabb a b)

  let test_aabb_aabb_separated () =
    let a = aabb_minmax 0. 0. 2. 2. in
    let b = aabb_minmax 3. 0. 4. 1. in
    check bool "separated" false (Aabb2d.intersects_aabb a b)

  let test_aabb_aabb_containment () =
    let outer = aabb_minmax 0. 0. 10. 10. in
    let inner = aabb_minmax 2. 2. 3. 3. in
    check bool "containment is intersect" true (Aabb2d.intersects_aabb outer inner)

  let test_aabb_aabb_degenerate_point () =
    (* point at (2,2) intersecting a box that contains it *)
    let box = aabb_minmax 0. 0. 4. 4. in
    let pt = aabb_minmax 2. 2. 2. 2. in

    check bool "point inside" true (Aabb2d.intersects_aabb box pt);

    let pt2 = aabb_minmax 5. 5. 5. 5. in
    check bool "point outside" false (Aabb2d.intersects_aabb box pt2)

  let test_aabb_circle_inside () =
    let box = aabb_minmax 0. 0. 5. 5. in
    let c = circle 5.5 5.5 1.0 in
    check bool "circle fully inside box" true (Aabb2d.intersects_circle box c)

  let test_aabb_circle_touching_edge () =
    let box = aabb_minmax 0. 0. 4. 4. in

    (* circle centered at (5,2) radius=1 touches right edge x=4 *)
    let c = circle 5. 2. 1. in
    check bool "touching edge" true (Aabb2d.intersects_circle box c)

  let test_aabb_circle_overlap_general () =
    let box = aabb_minmax 0. 0. 4. 4. in
    let c = circle 4.5 4.5 1.0 in
    check bool "overlap (corner region)" true (Aabb2d.intersects_circle box c)

  let test_aabb_circle_separated () =
    let box = aabb_minmax 0. 0. 2. 2. in
    let c = circle 5. 5. 1.0 in
    check bool "no intersection" false (Aabb2d.intersects_circle box c)

  let test_circle_circle_overlap () =
    let c1 = circle 0. 0. 2. in
    let c2 = circle 3. 0. 2. in
    check bool "overlapping" true (Bounding_circle.intersects_circle c1 c2)

  let test_circle_circle_touching () =
    let c1 = circle 0. 0. 1. in
    let c2 = circle 2. 0. 1. in
    check bool "touching edges" true (Bounding_circle.intersects_circle c1 c2)

  let test_circle_circle_contained () =
    let c1 = circle 0. 0. 3. in
    let c2 = circle 1. 0. 1. in
    check bool "one inside other" true (Bounding_circle.intersects_circle c1 c2)

  let test_circle_circle_separated () =
    let c1 = circle 0. 0. 1. in
    let c2 = circle 5. 0. 1. in
    check bool "separated" false (Bounding_circle.intersects_circle c1 c2)

  let test_circle_aabb_inside () =
    let box = aabb_minmax 0. 0. 5. 5. in
    let c = circle 2.5 2.5 1. in
    check bool "circle inside box" true (Bounding_circle.intersects_aabb c box)

  let test_circle_aabb_touching () =
    let box = aabb_minmax 0. 0. 4. 4. in
    let c = circle 5. 2. 1. in
    check bool "touching edge" true (Bounding_circle.intersects_aabb c box)

  let test_circle_aabb_corner_overlap () =
    let box = aabb_minmax 0. 0. 4. 4. in
    let c = circle 4.5 4.5 1. in
    check bool "touching corner" true (Bounding_circle.intersects_aabb c box)

  let test_circle_aabb_separated () =
    let box = aabb_minmax 0. 0. 4. 4. in
    let c = circle 7. 7. 1. in
    check bool "completely outside" false (Bounding_circle.intersects_aabb c box)

  let test_bounding_circle_identity_rotation_does_not_zero_center () =
    let iso = Isometry.create Rot2.identity Vec2.zero in
    let pts = [| v 1. 0.; v 3. 0. |] in
    (* mean should be (2,0) *)
    let c = Bounding_circle.of_point_cloud iso pts in
    check_vec "center" (Bounding_circle.center c) (v 2. 0.);
    check_float "radius" 1.0 (Bounding_circle.radius c)

  let test_bounding_circle_of_point_cloud_centroid_identity () =
    let iso = Isometry.create Rot2.identity Vec2.zero in
    let pts = [| v 0. 0.; v 2. 0.; v 0. 2. |] in
    (* centroid = (2/3, 2/3); max dist = sqrt(20)/3 *)
    let c = Bounding_circle.of_point_cloud iso pts in
    check_vec "center" (Bounding_circle.center c) (v (2. /. 3.) (2. /. 3.));
    check_float "radius" (Float.sqrt 20. /. 3.) (Bounding_circle.radius c)

  let test_bounding_circle_of_point_cloud_centroid_rot_trans () =
    let iso = Isometry.create Rot2.half_pi (v 10. (-5.)) in
    let pts = [| v 1. 0.; v 3. 0.; v 1. 2. |] in
    (* local centroid = (5/3, 2/3)
         rotate 90deg: (x,y)->(-y,x) => (-2/3, 5/3)
         then translate (10,-5) *)
    let expected_center = v (10. -. (2. /. 3.)) (-5. +. (5. /. 3.)) in
    let expected_radius = Float.sqrt 20. /. 3. in
    let c = Bounding_circle.of_point_cloud iso pts in
    check_vec "center" (Bounding_circle.center c) expected_center;
    check_float "radius" expected_radius (Bounding_circle.radius c)
end

module Direction = struct
  open Direction
  open Luma__math

  let test_create () =
    let open Dir2 in
    (* OK: normalizes (3,4) -> (0.6, 0.8), len = 5 *)
    (match create_and_length (Vec2.create 3. 4.) with
    | Ok (dir, len) ->
        check_float "len (3,4)" 5.0 len;
        check_vec "dir (3,4) -> (0.6,0.8)" dir (Vec2.create 0.6 0.8)
    | Error _ -> Alcotest.fail "expected Ok for finite non-zero vector");

    (* Error Zero: zero vector *)
    (match create Vec2.zero with
    | Error (`Invalid_direction Zero) -> ()
    | Ok _ -> Alcotest.fail "expected Error Zero for zero vector"
    | Error (`Invalid_direction Infinite) -> Alcotest.fail "got Infinite for zero vector"
    | Error (`Invalid_direction NaN) -> Alcotest.fail "got NaN for zero vector"
    | _ -> Alcotest.fail "Got unexpected error");

    (* 3) Error NaN: any NaN component -> NaN length *)
    (match create (Vec2.create Float.nan 0.) with
    | Error (`Invalid_direction NaN) -> ()
    | Ok _ -> Alcotest.fail "expected Error NaN"
    | Error (`Invalid_direction Zero) -> Alcotest.fail "got Zero for NaN input"
    | Error (`Invalid_direction Infinite) -> Alcotest.fail "got Infinite for NaN input"
    | _ -> Alcotest.fail "Got unexpected error");

    (* Error Infinite: any infinite component -> infinite length *)
    (match create (Vec2.create Float.infinity 1.) with
    | Error (`Invalid_direction Infinite) -> ()
    | Ok _ -> Alcotest.fail "expected Error Infinite"
    | Error (`Invalid_direction Zero) -> Alcotest.fail "got Zero for Infinite input"
    | Error (`Invalid_direction NaN) -> Alcotest.fail "got NaN for Infinite input"
    | _ -> Alcotest.fail "Got unexpected error");
    ()
end

module Ray = struct
  open Ray.Ray2d

  let test_intersect_plane_basic () =
    (* Plane: y = 0, normal = (0,1) *)
    let plane_origin = v 0. 0. in
    let plane = Primitives.Plane2d.{ normal = v 0. 1. } in

    (* Ray from (0,-5) pointing up hits plane at t=5 *)
    let ray = create (v 0. (-5.)) (v 0. 1.) in
    check_opt_float "ray below, upward" (Some 5.0) (intersect_plane ray plane_origin plane);

    (* Ray from (0,5) pointing down hits at t=5 *)
    let ray2 = create (v 0. 5.) (v 0. (-1.)) in
    check_opt_float "ray above, downward" (Some 5.0) (intersect_plane ray2 plane_origin plane);

    (* Ray parallel (horizontal): no intersection *)
    let ray3 = create (v 0. 5.) (v 1. 0.) in
    check_opt_float "parallel ray" None (intersect_plane ray3 plane_origin plane);

    (* Ray pointing away (upward from above): no intersection *)
    let ray4 = create (v 0. 5.) (v 0. 1.) in
    check_opt_float "away direction" None (intersect_plane ray4 plane_origin plane);
    ()

  let test_intersect_plane_offset () =
    (* Plane: x = 2, normal = (1,0) *)
    let plane_origin = v 2. 0. in
    let plane = Primitives.Plane2d.{ normal = v 1. 0. } in

    (* Ray from (0,0) → rightward intersects at t=2 *)
    let ray = create (v 0. 0.) (v 1. 0.) in
    check_opt_float "rightward hit" (Some 2.0) (intersect_plane ray plane_origin plane);

    (* Ray from (3,0) → rightward (pointing away) → none *)
    let ray2 = create (v 3. 0.) (v 1. 0.) in
    check_opt_float "away from plane" None (intersect_plane ray2 plane_origin plane);

    (* Ray from (3,0) → leftward → t=1 *)
    let ray3 = create (v 3. 0.) (v (-1.) 0.) in
    check_opt_float "leftward hit" (Some 1.0) (intersect_plane ray3 plane_origin plane)

  let test_aabb_hit_center () =
    let open Bounded2d in
    let aabb = Aabb2d.of_center_halfsize (v 0. 0.) (v 1. 1.) in
    let ray = Raycast2d.from_ray (Ray.Ray2d.create (v (-2.) 0.) (v 1. 0.)) 10. in
    check_opt_float "hit center" (Some 1.0) (Raycast2d.aabb_intersection_at ray aabb)

  let test_aabb_parallel_miss () =
    (* Ray is horizontal at y=3, outside the AABB’s vertical slab -> miss *)
    let open Bounded2d in
    let aabb = Aabb2d.of_center_halfsize (v 0. 0.) (v 1. 1.) in
    let ray = Raycast2d.from_ray (Ray.Ray2d.create (v (-2.) 3.) (v 1. 0.)) 10. in
    check_opt_float "parallel miss (y outside slab)" None (Raycast2d.aabb_intersection_at ray aabb)

  let test_aabb_parallel_overlap_hit () =
    (* Ray is horizontal but within the AABB’s vertical slab -> should hit *)
    let open Bounded2d in
    let aabb = Aabb2d.of_center_halfsize (v 0. 0.) (v 1. 1.) in
    let ray = Raycast2d.from_ray (Ray.Ray2d.create (v (-2.) 0.5) (v 1. 0.)) 10. in
    check_opt_float "parallel overlap hit" (Some 1.0) (Raycast2d.aabb_intersection_at ray aabb)

  let test_aabb_inside_box () =
    (* Origin inside box -> t = 0.0 *)
    let open Bounded2d in
    let aabb = Aabb2d.of_center_halfsize (v 0. 0.) (v 1. 1.) in
    let ray = Raycast2d.from_ray (Ray.Ray2d.create (v 0. 0.) (v 1. 0.)) 10. in
    check_opt_float "starts inside" (Some 0.0) (Raycast2d.aabb_intersection_at ray aabb)

  let test_aabb_max_cull () =
    (* Entry at x=4 → t=4.0; max=10 -> hit, max=3 -> culled *)
    let open Bounded2d in
    let aabb = Aabb2d.of_center_halfsize (v 5. 0.) (v 1. 1.) in
    let ray_ok = Raycast2d.from_ray (Ray.Ray2d.create (v 0. 0.) (v 1. 0.)) 10. in
    let ray_cut = Raycast2d.from_ray (Ray.Ray2d.create (v 0. 0.) (v 1. 0.)) 3. in
    check_opt_float "within max" (Some 4.0) (Raycast2d.aabb_intersection_at ray_ok aabb);
    check_opt_float "beyond max" None (Raycast2d.aabb_intersection_at ray_cut aabb)

  let test_circle_hit_from_left () =
    (* Circle center at (0,0), r=1; ray from (-2,0) -> (1,0) *)
    let circle = Bounded2d.Bounding_circle.create (v 0. 0.) 1. in
    let ray = Raycast2d.from_ray (Ray.Ray2d.create (v (-2.) 0.) (v 1. 0.)) 10. in
    (* Entry when x = -1, so t = 1 *)
    check_opt_float "hit from left" (Some 1.0) (Raycast2d.circle_intersection_at ray circle)

  let test_circle_pointing_away_miss () =
    (* Start left of circle but pointing further left → projected > 0, and projected^2 > d2 ⇒ miss *)
    let circle = Bounded2d.Bounding_circle.create (v 0. 0.) 1. in
    let ray = Raycast2d.from_ray (Ray.Ray2d.create (v (-2.) 0.) (v (-1.) 0.)) 10. in
    check_opt_float "pointing away" None (Raycast2d.circle_intersection_at ray circle)

  let test_circle_start_inside () =
    (* Inside circle at origin, shoot any direction → t = 0 *)
    let circle = Bounded2d.Bounding_circle.create (v 0. 0.) 1. in
    let ray = Raycast2d.from_ray (Ray.Ray2d.create (v 0. 0.) (v 1. 0.)) 10. in
    check_opt_float "start inside" (Some 0.0) (Raycast2d.circle_intersection_at ray circle)
end

module Polygon = struct
  let test_polygon_area () =
    let open Primitives in
    let p =
      {
        Polygon.points =
          [| { x = 0.; y = 0. }; { x = 1.; y = 0. }; { x = 1.; y = 1. }; { x = 0.; y = 1. } |];
      }
    in
    let area = Polygon.area p in
    check_float "area is 1.0" 1. area;
    ()
end

let tests =
  ( "math",
    [
      "Rot2.t creation" -: Rotation.test_creation;
      "Rot2.t rotation" -: Rotation.test_rotate;
      "Rot2.t normalise" -: Rotation.test_normalise;
      "Circle.t closest_point" -: Circle.test_circle_closest_point;
      "Circle.t closest_point_offset_center" -: Circle.test_circle_closest_point_offset_center;
      "Vec2.t distance" -: Vec2.test_distance;
      "Vec2.t distance_squared" -: Vec2.test_distance_squared;
      "Aabb2d.t of_center_halfsize" -: Bounded2d_tests.test_of_center_halfsize;
      "Aabb2d.t of_min_max" -: Bounded2d_tests.test_of_min_max;
      "Aabb2d.t visible_area" -: Bounded2d_tests.test_visible_area;
      "Aabb2d.t contains_merge" -: Bounded2d_tests.test_contains_merge;
      "Aabb2d.t grow / shrink" -: Bounded2d_tests.test_grow_shrink;
      "Aabb2d.t bounding_circle" -: Bounded2d_tests.test_bounding_circle;
      "Bounding_circle.t circle_aabb_roundtrip_contains"
      -: Bounded2d_tests.test_circle_aabb_roundtrip_contains;
      "Aabb2d.t aabb_intersects_aabb" -: Bounded2d_tests.test_aabb_aabb_overlap;
      "Aabb2d.t aabb_aabb_touch_edge" -: Bounded2d_tests.test_aabb_aabb_touch_edge;
      "Aabb2d.t aabb_aabb_touch_corner" -: Bounded2d_tests.test_aabb_aabb_touch_corner;
      "Aabb2d.t aabb_aabb_separated" -: Bounded2d_tests.test_aabb_aabb_separated;
      "Aabb2d.t aabb_aabb_containment" -: Bounded2d_tests.test_aabb_aabb_containment;
      "Aabb2d.t aabb_circle_inside" -: Bounded2d_tests.test_aabb_circle_inside;
      "Aabb2d.t aabb_circle_touching_edge" -: Bounded2d_tests.test_aabb_circle_touching_edge;
      "Aabb2d.t aabb_circle_overlap_general" -: Bounded2d_tests.test_aabb_circle_overlap_general;
      "Aabb2d.t aabb_circle_no_intersection" -: Bounded2d_tests.test_aabb_circle_separated;
      "Aabb2d.t circle_circle_overlap" -: Bounded2d_tests.test_circle_circle_overlap;
      "Aabb2d.t circle_circle_touching" -: Bounded2d_tests.test_circle_circle_touching;
      "Aabb2d.t circle_circle_contained" -: Bounded2d_tests.test_circle_circle_contained;
      "Aabb2d.t circle_aabb_inside" -: Bounded2d_tests.test_circle_aabb_inside;
      "Aabb2d.t circle_aabb_touching" -: Bounded2d_tests.test_circle_aabb_touching;
      "Aabb2d.t circle_aabb_corner_overlap" -: Bounded2d_tests.test_circle_aabb_corner_overlap;
      "Aabb2d.t circle_aabb_separated" -: Bounded2d_tests.test_circle_aabb_separated;
      "Bounding_circle.t bounding_circle_identity_rotation_does_not_zero_center"
      -: Bounded2d_tests.test_bounding_circle_identity_rotation_does_not_zero_center;
      "Bounding_circle.t bounding_circle_of_point_cloud_centroid_identity"
      -: Bounded2d_tests.test_bounding_circle_of_point_cloud_centroid_identity;
      "Bounding_circle.t bounding_circle_of_point_cloud_centroid_rot_trans"
      -: Bounded2d_tests.test_bounding_circle_of_point_cloud_centroid_rot_trans;
      "Dir2.t create" -: Direction.test_create;
      "Ray2d.t basic" -: Ray.test_intersect_plane_basic;
      "Ray2d.t intersect_plane_offset" -: Ray.test_intersect_plane_offset;
      "Raycast2d.t aabb_hit_center" -: Ray.test_aabb_hit_center;
      "Raycast2d.t aabb_parallel_miss" -: Ray.test_aabb_parallel_miss;
      "Raycast2d.t aabb_parallel_overlap_hit" -: Ray.test_aabb_parallel_overlap_hit;
      "Raycast2d.t aabb_inside_box" -: Ray.test_aabb_inside_box;
      "Raycast2d.t aabb_max_cull" -: Ray.test_aabb_max_cull;
      "Raycast2d.t circle_hit_from_left" -: Ray.test_circle_hit_from_left;
      "Raycast2d.t circle_pointing_away_miss" -: Ray.test_circle_pointing_away_miss;
      "Raycast2d.t circle_start_inside" -: Ray.test_circle_start_inside;
      "Polygon.t area" -: Polygon.test_polygon_area;
    ] )
