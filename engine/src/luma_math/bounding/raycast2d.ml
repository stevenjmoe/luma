type t = {
  ray : Ray.Ray2d.t;
  max : float;
  direction_recip : Vec2.t;
}

let from_ray ray max = { ray; max; direction_recip = Vec2.recip ray.direction }
let create origin direction max = from_ray (Ray.Ray2d.create origin direction) max

(* Helpers to avoid propagating NaN from Float.min/max *)
let max_num a b = if Float.is_nan a then b else if Float.is_nan b then a else Float.max a b
let min_num a b = if Float.is_nan a then b else if Float.is_nan b then a else Float.min a b

let aabb_intersection_at ray (aabb : Bounded2d.Aabb2d.t) =
  let open Bounded2d.Aabb2d in
  let min_a, max_a = (min aabb, max aabb) in
  let min_x, max_x = if ray.ray.direction.x >= 0. then (min_a.x, max_a.x) else (max_a.x, min_a.x) in
  let min_y, max_y = if ray.ray.direction.y >= 0. then (min_a.y, max_a.y) else (max_a.y, min_a.y) in

  let tmin_x = (min_x -. ray.ray.origin.x) *. ray.direction_recip.x in
  let tmax_x = (max_x -. ray.ray.origin.x) *. ray.direction_recip.x in
  let tmin_y = (min_y -. ray.ray.origin.y) *. ray.direction_recip.y in
  let tmax_y = (max_y -. ray.ray.origin.y) *. ray.direction_recip.y in

  (* entry = largest of min values; exit = smallest of max values *)
  let tmin = max_num (max_num tmin_x tmin_y) 0. in
  let tmax = min_num (min_num tmax_y tmax_x) ray.max in

  if tmin <= tmax then Some tmin else None

let circle_intersection_at ray (circle : Bounded2d.Bounding_circle.t) =
  let open Bounded2d.Bounding_circle in
  let offset = Vec2.sub ray.ray.origin (center circle) in
  let projected = Vec2.dot offset ray.ray.direction in
  let cross = Vec2.perp_dot offset ray.ray.direction in
  let distance_squared = (radius circle *. radius circle) -. (cross *. cross) in
  if
    distance_squared < 0.
    || Float.copy_sign (projected *. projected) (-.projected) < -.distance_squared
  then None
  else
    let toi = -.projected -. Float.sqrt distance_squared in
    if toi > ray.max then None else Some (max_num toi 0.)

let intersects_aabb ray aabb = Option.is_some (aabb_intersection_at ray aabb)
let intersects_circle ray circle = Option.is_some (circle_intersection_at ray circle)
