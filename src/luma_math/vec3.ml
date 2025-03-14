type t = { x : float; y : float; z : float }

let create x y z = { x; y; z }
let zero () = { x = 0.; y = 0.; z = 0. }
let splat v = { x = v; y = v; z = v }
