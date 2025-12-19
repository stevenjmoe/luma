type t = {
  mutable x : float;
  mutable y : float;
}

let create x y = { x; y }
let zero = create 0. 0.
let left = create (-1.) 0.
let right = create 1. 0.
let up = create 0. (-1.)
let down = create 0. 1.

(** A vector which points along the positive x axis. *)
let pos_x = { x = 1.; y = 0. }

(** A vector which points along the positive y axis. *)
let pos_y = { x = 0.; y = 1. }

(** A vector which points along the negative x axis. *)
let neg_x = { x = -1.; y = 0. }

(** A vector which points along the negative y axis. *)
let neg_y = { x = 0.; y = -1. }

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

let div t rhs =
  let x = t.x /. rhs.x in
  let y = t.y /. rhs.y in
  { x; y }

let scale v k = create (k *. x v) (k *. y v)
let length v = Float.sqrt ((v.x *. v.x) +. (v.y *. v.y))
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

let max v1 v2 =
  { x = (if v1.x > v2.x then v1.x else v2.x); y = (if v1.y > v2.y then v1.y else v2.y) }

let min v1 v2 =
  { x = (if v1.x < v2.x then v1.x else v2.x); y = (if v1.y < v2.y then v1.y else v2.y) }

let clamp min_ max_ v = max min_ (min v max_)

(** [is_finite v] is [true] if and only if all elements are finite i.e., not infinite and not [nan].
*)
let is_finite v = Float.is_finite v.x && Float.is_finite v.y

let dot a b = (a.x *. b.x) +. (a.y *. b.y)

(** [perp_dot a b] The perpendicular dot product of [a] and [b] *)
let perp_dot a b = (a.x *. b.y) -. (a.y *. b.x)

(** [recip v] returns the component-wise reciprocal of [v]. *)
let recip v = create (1. /. v.x) (1. /. v.y)

let normalise v =
  let len = length v in
  if len > 0. then create (v.x /. len) (v.y /. len) else create 0. 0.

let rotate v theta =
  let c = cos theta and s = sin theta in
  let x = (v.x *. c) -. (v.y *. s) in
  let y = (v.x *. s) +. (v.y *. c) in
  { x; y }

module Infix = struct
  let ( *.. ) a b = mul a b
  let ( +.. ) a b = add a b
  let ( -.. ) a b = sub a b
end
