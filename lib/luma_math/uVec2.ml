(*TODO : assert unsigned ints *)
type t = { mutable x : int; mutable y : int }

let zero () = { x = 0; y = 0 }

let create x y =
  assert (x >= 0);
  assert (y >= 0);
  { x; y }

let splat v =
  assert (v >= 0);
  { x = v; y = v }

let x t = t.x
let y t = t.y

let mul t rhs =
  let x = t.x * rhs.x in
  let y = t.y * rhs.y in
  { x; y }

let add t rhs =
  let x = t.x + rhs.x in
  let y = t.y + rhs.y in
  { x; y }

let sub t rhs =
  let x = t.x - rhs.x in
  let y = t.y - rhs.y in
  { x; y }

module Infix = struct
  let ( *.. ) a b = mul a b
  let ( +.. ) a b = add a b
  let ( -.. ) a b = sub a b
end
