type t = {
  mutable x : float;
  mutable y : float;
  mutable z : float;
}

(** [create x y z] *)
let create x y z = { x; y; z }

let x v = v.x
let y v = v.y
let z v = v.z
let zero () = { x = 0.; y = 0.; z = 0. }
let splat v = { x = v; y = v; z = v }
let scale k v = create (k *. v.x) (k *. v.y) (k *. v.z)
let length v = Float.sqrt ((v.x *. v.x) +. (v.y *. v.y) +. (v.z *. v.z))
let length_squared v = (v.x *. v.x) +. (v.y *. v.y) +. (v.z *. v.z)

let normalise v =
  let len = length v in
  if len > 0. then create (v.x /. len) (v.y /. len) (v.z /. len) else create 0. 0. 0.
