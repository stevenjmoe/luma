type t = {
  mutable x : float;
  mutable y : float;
}

let create x y = { x; y }
let zero = { x = 0.; y = 0. }
let splat v = { x = v; y = v }
let x t = t.x
let y t = t.y

let abs t =
  let x = Float.abs t.x in
  let y = Float.abs t.y in
  create x y

let set_x t x = t.x <- x
let set_y t y = t.y <- y

let mul t rhs =
  let x = t.x *. rhs.x in
  let y = t.y *. rhs.y in
  { x; y }

let add t rhs =
  let x = t.x +. rhs.x in
  let y = t.y +. rhs.y in
  { x; y }

let sub t rhs =
  let x = t.x -. rhs.x in
  let y = t.y -. rhs.y in
  { x; y }

let scale k v = create (k *. x v) (k *. y v)
let length_squared v = (v.x *. v.x) +. (v.y *. v.y)

(** [distance vec1 vec2] computes the euclidean distance between two vectors *)
let distance v1 v2 =
  let dx = v1.x -. v2.x in
  let dy = v1.y -. v2.y in
  Float.sqrt ((dx *. dx) +. (dy *. dy))

(** [distance_squared vec1 vec2] computes the euclidean distance between two vectors.

    Cheaper than [distance] because it avoids [Float.sqrt]. *)
let distance_squared v1 v2 =
  let dx = v1.x -. v2.x in
  let dy = v1.y -. v2.y in
  (dx *. dx) +. (dy *. dy)

module Infix = struct
  let ( *.. ) a b = mul a b
  let ( +.. ) a b = add a b
  let ( -.. ) a b = sub a b
end
