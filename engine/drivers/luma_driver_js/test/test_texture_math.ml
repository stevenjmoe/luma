open Alcotest
open Luma__math

let ( -: ) n f = Alcotest.test_case n `Quick f
let eps = 1e-6
let checkf name expected actual = check (float eps) name expected actual
let mk_rect x y w h = Rect.create ~pos:(Vec2.create x y) ~size:(Vec2.create w h)

module M = Luma_driver_js.Texture_math

let no_flip_plan () =
  let src = mk_rect 10. 20. 30. 40. in
  let dst = mk_rect 100. 200. 300. 400. in
  let origin = Vec2.create 5. 6. in
  let p = M.plan ~src ~dst ~origin in

  check bool "flip_x" false p.flip_x;
  check bool "flip_y" false p.flip_y;
  checkf "sx" 10. p.sx;
  checkf "sy" 20. p.sy;
  checkf "sw" 30. p.sw;
  checkf "sh" 40. p.sh;
  checkf "draw_x" (-5.) p.draw_x;
  checkf "draw_y" (-6.) p.draw_y

let flip_x_preserves_src_origin () =
  let src = mk_rect 10. 20. (-30.) 40. in
  let dst = mk_rect 100. 200. 300. 400. in
  let origin = Vec2.create 7. 9. in
  let p = M.plan ~src ~dst ~origin in

  check bool "flip_x" true p.flip_x;
  check bool "flip_y" false p.flip_y;
  checkf "sx" 10. p.sx;
  checkf "sy" 20. p.sy;
  checkf "sw" 30. p.sw;
  checkf "sh" 40. p.sh;
  checkf "draw_x" (-293.) p.draw_x;
  checkf "draw_y" (-9.) p.draw_y

let flip_xy_uses_adjusted_origin () =
  let src = mk_rect 10. 20. (-30.) (-40.) in
  let dst = mk_rect 100. 200. 300. 400. in
  let origin = Vec2.create 7. 9. in
  let p = M.plan ~src ~dst ~origin in

  check bool "flip_x" true p.flip_x;
  check bool "flip_y" true p.flip_y;
  checkf "sx" (-20.) p.sx;
  checkf "sy" (-20.) p.sy;
  checkf "sw" 30. p.sw;
  checkf "sh" 40. p.sh;
  checkf "draw_x" (-293.) p.draw_x;
  checkf "draw_y" (-391.) p.draw_y

let should_draw_contract () =
  let origin = Vec2.create 0. 0. in
  let src = mk_rect 0. 0. 10. 10. in
  let dst = mk_rect 0. 0. 20. 20. in
  let p = M.plan ~src ~dst ~origin in
  check bool "draw positive sizes" true (M.should_draw p ~dst);

  let src_zero_w = mk_rect 0. 0. 0. 10. in
  let p = M.plan ~src:src_zero_w ~dst ~origin in
  check bool "dont draw zero source width" false (M.should_draw p ~dst);

  let dst_zero_h = mk_rect 0. 0. 20. 0. in
  let p = M.plan ~src ~dst:dst_zero_h ~origin in
  check bool "dont draw zero dest height" false (M.should_draw p ~dst:dst_zero_h)

let tests =
  ( "texture_math",
    [
      "no_flip_plan" -: no_flip_plan;
      "flip_x_preserves_src_origin" -: flip_x_preserves_src_origin;
      "flip_xy_uses_adjusted_origin" -: flip_xy_uses_adjusted_origin;
      "should_draw_contract" -: should_draw_contract;
    ] )
