type t = {
  x : float;
  y : float;
  z : float;
}

(** [create x y z] *)
let create x y z = { x; y; z }

let x v = v.x
let y v = v.y
let z v = v.z
let zero () = { x = 0.; y = 0.; z = 0. }
let splat v = { x = v; y = v; z = v }
