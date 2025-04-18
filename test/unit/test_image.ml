module I = Luma__image.Image
module E = Luma__ecs
module Math = Luma__math
open Alcotest

let ( -: ) n f = Alcotest.test_case n `Quick f

module Position = struct
  type t = { x : float; y : float }

  module C = E.Component.Make (struct
    type inner = t
  end)
end

let test_image_min () =
  let open Luma__ecs.Query in
  let image = I.Texture_atlas_layout.empty () in
  let e = epsilon_float in

  check (float e) "entity" 0. (I.Texture_atlas_layout.size image).x;
  check (float e) "entity" 0. (I.Texture_atlas_layout.size image).y

let test_texture_atlas_layout () =
  let open Luma__ecs.Query in
  let tile_size = Math.Vec2.create 48. 48. in
  let layout = I.Texture_atlas_layout.from_grid tile_size 8 3 in

  check bool "Check the layout has textures" false (I.Texture_atlas_layout.is_empty layout);
  check int "Check that the correct number of textures are present." 24
    (I.Texture_atlas_layout.length layout);
  ()

let tests = ("image", [ "min" -: test_image_min; "from grid" -: test_texture_atlas_layout ])
