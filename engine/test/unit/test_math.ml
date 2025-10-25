open Alcotest
open Luma__math

let ( -: ) n f = Alcotest.test_case n `Quick f
let e = 1e-6

let check_vec name a b =
  let open Vec2 in
  Alcotest.(check (float 1e-6)) (name ^ " x") b.x a.x;
  Alcotest.(check (float 1e-6)) (name ^ " y") b.y a.y

let check_float name expected actual = Alcotest.(check (float e)) name expected actual

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

    let point_in = Vec2.create 1.0 1.0 in
    check_vec "inside" (closest_point circle point_in) point_in;

    let point_on = Vec2.create 2.0 0.0 in
    check_vec "on-boundary" (closest_point circle point_on) point_on;

    let point_out_x = Vec2.create 3.0 0.0 in
    let expect_x = Vec2.create 2.0 0.0 in
    check_vec "outside axis" (closest_point circle point_out_x) expect_x;

    let point_out_diag = Vec2.create 3.0 4.0 in
    let expect_diag = Vec2.create 1.2 1.6 in
    check_vec "outside diagonal" (closest_point circle point_out_diag) expect_diag;

    let point_zero = Vec2.zero in
    check_vec "origin" (closest_point circle point_zero) point_zero
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

module Bounded2d = struct
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
end

let tests =
  ( "math",
    [
      "Rot2.t creation" -: Rotation.test_creation;
      "Rot2.t rotation" -: Rotation.test_rotate;
      "Rot2.t normalise" -: Rotation.test_normalise;
      "Circle.t closest_point" -: Circle.test_circle_closest_point;
      "Vec2.t distance" -: Vec2.test_distance;
      "Vec2.t distance_squared" -: Vec2.test_distance_squared;
      "Aabb2d.t of_center_halfsize" -: Bounded2d.test_of_center_halfsize;
      "Aabb2d.t of_min_max" -: Bounded2d.test_of_min_max;
      "Aabb2d.t visible_area" -: Bounded2d.test_visible_area;
      "Aabb2d.t contains_merge" -: Bounded2d.test_contains_merge;
      "Aabb2d.t grow / shrink" -: Bounded2d.test_grow_shrink;
      "Aabb2d.t bounding_circle" -: Bounded2d.test_bounding_circle;
      "Bounding_circle.t circle_aabb_roundtrip_contains"
      -: Bounded2d.test_circle_aabb_roundtrip_contains;
    ] )
