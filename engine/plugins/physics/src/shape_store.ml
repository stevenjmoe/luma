open Luma__math

type shape = Rigid_body.shape =
  | Circle of float
  | Aabb of Vec2.t
  | Polygon of Vec2.t array

type t = {
  mutable len : int;
  mutable cap : int;
  mutable shape_kind : int array;
  mutable circle_radius : float array;
  mutable box_hw : float array;
  mutable box_hh : float array;
  mutable poly_offset : int array;
  mutable poly_count : int array;
  mutable poly_x : float array;
  mutable poly_y : float array;
  mutable poly_len : int;
  mutable poly_cap : int;
}

let shape_circle = 0
let shape_aabb = 1
let shape_polygon = 2

let create ?(initial = 128) () =
  let f v = Array.make initial v in
  let initial_poly_cap = max 64 initial in
  {
    len = 0;
    cap = initial;
    shape_kind = f shape_circle;
    circle_radius = f 0.;
    box_hw = f 0.;
    box_hh = f 0.;
    poly_offset = f 0;
    poly_count = f 0;
    poly_x = Array.make initial_poly_cap 0.;
    poly_y = Array.make initial_poly_cap 0.;
    poly_len = 0;
    poly_cap = initial_poly_cap;
  }

let len s = s.len

let ensure_capacity s need =
  if need <= s.cap then ()
  else
    let new_cap = max need (s.cap * 2) in
    let grow_float a =
      let b = Array.make new_cap 0. in
      Array.blit a 0 b 0 s.len;
      b
    in
    let grow_int a =
      let b = Array.make new_cap 0 in
      Array.blit a 0 b 0 s.len;
      b
    in
    let grow_points a =
      let b = Array.make new_cap 0 in
      Array.blit a 0 b 0 s.len;
      b
    in
    s.cap <- new_cap;
    s.shape_kind <- grow_int s.shape_kind;
    s.circle_radius <- grow_float s.circle_radius;
    s.box_hw <- grow_float s.box_hw;
    s.box_hh <- grow_float s.box_hh;
    s.poly_offset <- grow_points s.poly_offset;
    s.poly_count <- grow_points s.poly_count

let ensure_poly_capacity s need =
  if need <= s.poly_cap then ()
  else
    let new_cap = max need (s.poly_cap * 2) in
    let grow a =
      let b = Array.make new_cap 0. in
      Array.blit a 0 b 0 s.poly_len;
      b
    in
    s.poly_x <- grow s.poly_x;
    s.poly_y <- grow s.poly_y;
    s.poly_cap <- new_cap

let swap_rows s i j =
  if i = j then ()
  else
    let swap a =
      let t = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- t
    in
    swap s.shape_kind;
    swap s.circle_radius;
    swap s.box_hw;
    swap s.box_hh;
    swap s.poly_offset;
    swap s.poly_count

let add_circle s radius =
  let new_len = s.len + 1 in
  ensure_capacity s new_len;
  let i = s.len in
  s.shape_kind.(i) <- shape_circle;
  s.circle_radius.(i) <- radius;
  s.box_hw.(i) <- 0.;
  s.box_hh.(i) <- 0.;
  s.poly_offset.(i) <- 0;
  s.poly_count.(i) <- 0;
  s.len <- new_len;
  i

let add_aabb s (half_size : Vec2.t) =
  let new_len = s.len + 1 in
  ensure_capacity s new_len;
  let i = s.len in
  s.shape_kind.(i) <- shape_aabb;
  s.circle_radius.(i) <- 0.;
  s.box_hw.(i) <- half_size.x;
  s.box_hh.(i) <- half_size.y;
  s.poly_offset.(i) <- 0;
  s.poly_count.(i) <- 0;
  s.len <- new_len;
  i

let add_polygon s (points : Vec2.t array) =
  if Array.length points < 3 then invalid_arg "Shape_store.add_polygon: need at least 3 points";
  if not (Primitives.Polygon.is_convex Primitives.Polygon.{ points }) then
    invalid_arg "Shape_store.add_polygon: polygon must be convex";
  let count = Array.length points in
  let offset = s.poly_len in
  ensure_poly_capacity s (offset + count);

  let new_len = s.len + 1 in
  ensure_capacity s new_len;

  let i = s.len in
  s.shape_kind.(i) <- shape_polygon;
  s.circle_radius.(i) <- 0.;
  s.box_hw.(i) <- 0.;
  s.box_hh.(i) <- 0.;

  for p = 0 to count - 1 do
    let v = points.(p) in
    s.poly_x.(offset + p) <- v.x;
    s.poly_y.(offset + p) <- v.y
  done;

  s.poly_offset.(i) <- offset;
  s.poly_count.(i) <- count;
  s.poly_len <- offset + count;
  s.len <- new_len;
  i

let add s = function
  | Circle radius -> add_circle s radius
  | Aabb half_size -> add_aabb s half_size
  | Polygon points -> add_polygon s points

let remove s row =
  let last = s.len - 1 in
  if row < 0 || row >= s.len then invalid_arg "Shape_store.remove";
  if row < last then swap_rows s row last;
  s.len <- last

let clear s =
  s.len <- 0;
  s.poly_len <- 0

let shape_kind s row = s.shape_kind.(row)
let is_circle s row = s.shape_kind.(row) = shape_circle
let is_aabb s row = s.shape_kind.(row) = shape_aabb
let is_polygon s row = s.shape_kind.(row) = shape_polygon
let circle_radius s row = s.circle_radius.(row)
let set_circle_radius s row radius = s.circle_radius.(row) <- radius
let aabb_half_size s row = Vec2.create s.box_hw.(row) s.box_hh.(row)
let set_aabb_half_width s row width = s.box_hw.(row) <- width
let set_aabb_half_height s row height = s.box_hh.(row) <- height

let set_aabb_half_size s row (half_size : Vec2.t) =
  s.box_hw.(row) <- half_size.x;
  s.box_hh.(row) <- half_size.y

let aabb_half_width s row = s.box_hw.(row)
let aabb_half_height s row = s.box_hh.(row)

let polygon_points s row =
  let count = s.poly_count.(row) in
  let offset = s.poly_offset.(row) in
  Array.init count (fun i -> Vec2.create s.poly_x.(offset + i) s.poly_y.(offset + i))

let polygon_points_x s row =
  let count = s.poly_count.(row) in
  let offset = s.poly_offset.(row) in
  Array.init count (fun i -> s.poly_x.(offset + i))

let polygon_points_y s row =
  let count = s.poly_count.(row) in
  let offset = s.poly_offset.(row) in
  Array.init count (fun i -> s.poly_y.(offset + i))

let polygon_offset s row = s.poly_offset.(row)
let polygon_count s row = s.poly_count.(row)
let polygon_storage_x s = s.poly_x
let polygon_storage_y s = s.poly_y

let polygon_points_copy s row = polygon_points s row

let shape_at s row =
  match s.shape_kind.(row) with
  | k when k = shape_circle -> Circle s.circle_radius.(row)
  | k when k = shape_aabb -> Aabb (Vec2.create s.box_hw.(row) s.box_hh.(row))
  | k when k = shape_polygon -> Polygon (polygon_points s row)
  | other -> invalid_arg (Printf.sprintf "Shape_store.shape_at %d" other)

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "shape_store"
end)
