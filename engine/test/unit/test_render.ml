open Alcotest
open Luma__render
open Luma__math

let vec2 =
  let open Vec2 in
  let pp fmt v = Format.fprintf fmt "(%g,%g)" v.x v.y in
  let eq a b = Float.equal a.x b.x && Float.equal a.y b.y in
  testable pp eq

let ( -: ) n f = Alcotest.test_case n `Quick f
let v x y = Vec2.create x y

module Projection = struct
  let window_to_viewport_basic () =
    let viewport_pos = v 100. 50. in
    let window_pos = v 130. 80. in
    let result = Render.Projection_pure.window_to_viewport_pos ~viewport_pos window_pos in

    check vec2 "window -> viewport" (v 30. 30.) result;
    ()

  let viewport_to_window_basic () =
    let viewport_pos = v 200. 300. in
    let viewport_local = v 10. 20. in

    let result = Render.Projection_pure.viewport_to_window_pos ~viewport_pos viewport_local in

    check vec2 "viewport -> window" (v 210. 320.) result;
    ()

  let round_trip_window_viewport () =
    let viewport_pos = v 400. 100. in
    let window_pos = v 512. 384. in

    let viewport_local = Render.Projection_pure.window_to_viewport_pos ~viewport_pos window_pos in
    let window_pos' = Render.Projection_pure.viewport_to_window_pos ~viewport_pos viewport_local in

    check vec2 "round-trip" window_pos window_pos';
    ()

  let negative_viewport_origin () =
    let viewport_pos = v (-100.) (-50.) in
    let window_pos = v 0. 0. in

    let result = Render.Projection_pure.window_to_viewport_pos ~viewport_pos window_pos in

    check vec2 "negative viewport origin" (v 100. 50.) result;
    ()
end

let tests =
  ( "render",
    [
      "window_to_viewport_basic" -: Projection.window_to_viewport_basic;
      "viewport_to_window_basic" -: Projection.viewport_to_window_basic;
      "round-trip" -: Projection.round_trip_window_viewport;
      "negative_viewport_origin" -: Projection.negative_viewport_origin;
    ] )
