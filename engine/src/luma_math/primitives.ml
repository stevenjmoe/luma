module Circle = struct
  type t = { radius : float }

  let default () = { radius = 0.5 }

  (** [create radius] *)
  let create radius = { radius }

  let diameter c = 2.0 *. c.radius
  let radius_squared c = c.radius *. c.radius

  let closest_point c (point : Vec2.t) =
    let dist = (point.x *. point.x) +. (point.y *. point.y) in
    if dist <= c.radius *. c.radius then point
    else
      let inv_len = 1.0 /. Float.sqrt dist in
      let dir = Vec2.create (point.x *. inv_len) (point.y *. inv_len) in
      Vec2.create (dir.x *. c.radius) (dir.y *. c.radius)
end
