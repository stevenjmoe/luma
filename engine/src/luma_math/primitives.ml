module Circle = struct
  type t = {
    radius : float;
    mutable center : Vec2.t;
  }

  (** [create radius center] *)
  let create radius center = { radius; center }

  let diameter c = 2.0 *. c.radius
  let radius c = c.radius
  let radius_squared c = c.radius *. c.radius
  let area c = Float.pi *. c.radius *. c.radius
  let center c = c.center

  let closest_point c (point : Vec2.t) =
    let dist = (point.x *. point.x) +. (point.y *. point.y) in
    if dist <= c.radius *. c.radius then point
    else
      let inv_len = 1.0 /. Float.sqrt dist in
      let dir = Vec2.create (point.x *. inv_len) (point.y *. inv_len) in
      Vec2.create (dir.x *. c.radius) (dir.y *. c.radius)
end

module Plane2d = struct
  type t = { normal : Direction.Dir2.t }

  (** Returns the default [Plane2d.t] with the normal pointing to [Dir2.pos_y]. *)
  let default () = { normal = Direction.Dir2.pos_y }

  let create normal =
    match Direction.Dir2.create normal with
    | Error _ -> failwith "Normal must be nonzero and finite"
    | Ok normal -> { normal }
end
