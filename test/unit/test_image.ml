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

let tests = ("image", [ "min" -: test_image_min; "from grid" -: test_texture_atlas_layout ])
