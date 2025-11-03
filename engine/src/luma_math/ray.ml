module Ray2d = struct
  type t = {
    origin : Vec2.t;
    direction : Direction.Dir2.t;
  }

  (**[create origin direction] *)
  let create origin direction = { origin; direction }

  (** [get_point ray distance] *)
  let get_point ray distance = Vec2.add ray.origin (Vec2.scale ray.direction distance)

  (** [intersect_plane ray plane_origin plane] *)
  let intersect_plane ray plane_origin (plane : Primitives.Plane2d.t) =
    let denominator = Vec2.dot plane.normal ray.direction in
    if Float.abs denominator > Float.epsilon then
      let distance = Vec2.dot (Vec2.sub plane_origin ray.origin) plane.normal /. denominator in
      if distance > Float.epsilon then Some distance else None
    else None
end
