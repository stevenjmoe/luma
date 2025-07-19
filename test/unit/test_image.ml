module Texture_atlas_layout = Luma__image.Texture_atlas_layout
module E = Luma__ecs
module Math = Luma__math
open Alcotest

let ( -: ) n f = Alcotest.test_case n `Quick f

module Position = struct
  type t = {
    x : float;
    y : float;
  }

  module C = E.Component.Make (struct
    type inner = t

    let name = "Position"
  end)
end

let test_image_min () =
  let image = Texture_atlas_layout.empty () in
  let e = epsilon_float in

  check (float e) "entity" 0. (Texture_atlas_layout.size image).x;
  check (float e) "entity" 0. (Texture_atlas_layout.size image).y

let test_texture_atlas_layout () =
  let tile_size = Math.Vec2.create 48. 48. in
  let layout = Texture_atlas_layout.from_grid tile_size 8 3 in

  check bool "Check the layout has textures" false (Texture_atlas_layout.is_empty layout);
  check int "Check that the correct number of textures are present." 24
    (Texture_atlas_layout.length layout);
  ()

let nine_texture_layout () =
  let tile_size = Math.Vec2.create 16. 16. in
  Texture_atlas_layout.from_grid tile_size 3 3

let test_last_index () =
  let layout = nine_texture_layout () in
  let last_index = Texture_atlas_layout.last_index layout in
  check (option int) "Check the last index is some 9" (Some 8) last_index;
  ()

let test_append_texture () =
  let layout = nine_texture_layout () in
  let new_rect =
    Math.Rect.create ~pos:(Math.Vec2.create 10. 10.) ~size:(Math.Vec2.create 10. 10.)
  in
  let layout = Texture_atlas_layout.append_texture new_rect layout in
  let last_index = Texture_atlas_layout.last_index layout in
  check (option int) "Check that adding a texture to the layout correctly increments the last_index"
    (Some 9) last_index;
  ()

let test_insert_texture () =
  let layout = nine_texture_layout () in
  let new_rect =
    Math.Rect.create ~pos:(Math.Vec2.create 10. 10.) ~size:(Math.Vec2.create 10. 10.)
  in
  let layout = Texture_atlas_layout.insert_texture new_rect 3 layout in
  let last_index = Texture_atlas_layout.last_index layout in

  check (option int)
    "Check that inserting a texture to the layout at index 3 correctly increments the last_index"
    (Some 9) last_index;

  check_raises
    "Check that attempting to insert a texture at an out of bounds position correctly raises"
    (Invalid_argument "insert_texture: index out of bounds") (fun () ->
      ignore (Texture_atlas_layout.insert_texture new_rect 11 layout));
  ()

let tests =
  ( "image",
    [
      "min" -: test_image_min;
      "from grid" -: test_texture_atlas_layout;
      "last index" -: test_last_index;
      "last index after appending texture" -: test_append_texture;
      "last index after inserting texture" -: test_insert_texture;
    ] )
