type t = {
  cos : float;
  sin : float;
}

let identity = { cos = 1.; sin = 0. }
let of_radians theta = { cos = Float.cos theta; sin = Float.sin theta }
let of_degrees deg = of_radians (deg *. Float.pi /. 180.)

(** [to_radians rot] returns a corresponding rotation angle in radians. *)
let to_radians rot = atan2 rot.sin rot.cos

let to_degrees rot = to_radians rot *. 180. /. Float.pi

(** [of_sin_cos sin cos] creates a [Rot2.t] from sine and cosine of an angle. *)
let of_sin_cos sin cos = { sin; cos }

(** Ensures the rotation has unit length.

    Use this to correct accumulated floating-point drift after repeated compositions. The result
    will have [cos² + sin² = 1]. *)
let normalise r =
  let m2 = (r.cos *. r.cos) +. (r.sin *. r.sin) in
  if Float.abs (m2 -. 1.) < 1e-9 then r
  else
    let inv_len = 1.0 /. Float.sqrt m2 in
    { cos = r.cos *. inv_len; sin = r.sin *. inv_len }

(** [compose a b] applies rotation [b] followed by [a].

    Mathematically equivalent to complex multiplication [(a.cos + i·a.sin) * (b.cos + i·b.sin)]. The
    result is renormalised to maintain unit length. *)
let compose a b =
  normalise
    { cos = (a.cos *. b.cos) -. (a.sin *. b.sin); sin = (a.sin *. b.cos) +. (a.cos *. b.sin) }

(** Returns the inverse of a rotation (its clockwise counterpart). *)
let inverse rot = { cos = rot.cos; sin = -.rot.sin }

(** A rotation of π radians. Corresponds to a half-turn. *)
let pi = of_radians Float.pi

(** Counterclockwise rotation of π/2 radians. Counterclockwise quarter-turn. *)
let half_pi = of_radians (Float.pi /. 2.)

(** Counterclockwise rotation of π/3 radians. Counterclockwise turn by 60 degrees .*)
let quarter_pi = of_radians (Float.pi /. 4.)
