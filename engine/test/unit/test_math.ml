open Alcotest
open Luma__math

let ( -: ) n f = Alcotest.test_case n `Quick f
let e = 1e-6

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

let check_vec name a b =
  let open Vec2 in
  Alcotest.(check (float 1e-6)) (name ^ " x") b.x a.x;
  Alcotest.(check (float 1e-6)) (name ^ " y") b.y a.y

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

  let point_zero = Vec2.create 0.0 0.0 in
  check_vec "origin" (closest_point circle point_zero) point_zero

let tests =
  ( "math",
    [
      "Rot2.t creation" -: test_creation;
      "Rot2.t rotation" -: test_rotate;
      "Rot2.t normalise" -: test_normalise;
      "Circle.t closest_point" -: test_circle_closest_point;
    ] )
