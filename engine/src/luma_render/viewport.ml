open Luma__math

type depth = {
  min : float;
  max : float;
}

type t = {
  physical_position : Vec2.t;
  physical_size : Vec2.t;
  depth : depth;
}

let make ~pos ~size depth_min depth_max =
  let clamp f = Float.max 0.0 (Float.min 1. f) in
  let depth = { min = clamp depth_min; max = clamp depth_max } in
  if Vec2.x size <= 0. || Vec2.y size <= 0. then invalid_arg "Viewport.make: size can't be 0";
  if depth.min > depth.max then invalid_arg "Viewport.make: depth.min > depth.max";
  { physical_position = pos; physical_size = size; depth }

let full w h = make ~pos:(Vec2.create 0. 0.) ~size:(Vec2.create (float w) (float h)) 0. 1.
let position v = v.physical_position
let size v = v.physical_size
let x v = Vec2.x v.physical_position
let y v = Vec2.y v.physical_position
let w v = Vec2.x v.physical_size
let h v = Vec2.y v.physical_size
let center v = Vec2.create (x v +. (0.5 *. w v)) (y v +. (0.5 *. h v))
let depth v = v.depth

let to_rect v =
  ( int_of_float v.physical_position.x,
    int_of_float v.physical_position.y,
    int_of_float v.physical_size.x,
    int_of_float v.physical_size.y )

let clamp_to_window w h v =
  let f_max f = Float.max 0. f in
  let x = f_max v.physical_position.x in
  let y = f_max v.physical_position.y in
  let w = Float.min v.physical_size.x (f_max (w -. x)) in
  let h = Float.min v.physical_size.y (f_max (h -. y)) in
  { v with physical_position = { x; y }; physical_size = { x = w; y = h } }
