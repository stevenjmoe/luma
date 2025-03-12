type t = { mutable x : float; mutable y : float }

let create x y = { x; y }
let zero = { x = 0.; y = 0. }
let splat v = { x = v; y = v }
let x t = t.x
let y t = t.y
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

module Infix = struct
  let ( *.. ) a b = mul a b
  let ( +.. ) a b = add a b
  let ( -.. ) a b = sub a b
end
