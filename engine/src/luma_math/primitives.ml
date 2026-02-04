module Circle = struct
  type t = { radius : float }

  (** [create radius] *)
  let create ~radius = { radius }

  let diameter c = 2.0 *. c.radius
  let radius c = c.radius
  let radius_squared c = c.radius *. c.radius
  let area c = Float.pi *. c.radius *. c.radius

  let closest_point c (point : Vec2.t) center =
    let d = Vec2.sub point center in
    let dist = (d.x *. d.x) +. (d.y *. d.y) in
    if dist <= c.radius *. c.radius then point
    else
      let inv_len = 1.0 /. Float.sqrt dist in
      let dir = Vec2.create (d.x *. inv_len) (d.y *. inv_len) in
      Vec2.add center (Vec2.mul dir (Vec2.splat c.radius))
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

module Capsule2d = struct
  type t = {
    radius : float;
    half_length : float;
  }

  let default () = { radius = 0.5; half_length = 0.5 }

  (** [create center radius length] creates a new [Capsule2d.t] from a radius and length. *)
  let create ~radius ~length =
    assert (radius >= 0.);
    assert (length >= 0.);
    { radius; half_length = length /. 2. }

  let top c center = Vec2.add center (Vec2.create 0. c.half_length)
  let bottom c center = Vec2.sub center (Vec2.create 0. c.half_length)

  let to_inner_rectangle c center =
    let size = Vec2.create (c.radius *. 2.) (c.half_length *. 2.) in
    let pos = Vec2.sub center (Vec2.mul size (Vec2.splat 0.5)) in
    Rect.create ~pos ~size
end

module Polygon = struct
  type t = { points : Vec2.t array }

  let signed_area p =
    let pts = p.points in
    let n = Array.length pts in
    if n < 3 then 0.0
    else
      let acc = ref 0.0 in
      for i = 0 to n - 1 do
        let j = (i + 1) mod n in
        acc := !acc +. ((pts.(i).x *. pts.(j).y) -. (pts.(j).x *. pts.(i).y))
      done;
      !acc /. 2.0

  let area pts = abs_float (signed_area pts)

  let is_convex p =
    let pts = p.points in
    let n = Array.length pts in
    if n < 3 then false
    else
      let dir = ref 0 in

      let sgn x = if x > Float.epsilon then 1 else if x < -.Float.epsilon then -1 else 0 in

      let rec loop i =
        (* no non-zero turn. polygon is collinear/degenerate *)
        if i = n then !dir <> 0
        else
          let cross ax ay bx by = (ax *. by) -. (ay *. bx) in

          let a = pts.(i) in
          let b = pts.((i + 1) mod n) in
          let c = pts.((i + 2) mod n) in

          let abx = b.x -. a.x in
          let aby = b.y -. a.y in
          let bcx = c.x -. b.x in
          let bcy = c.y -. b.y in

          let turn = cross abx aby bcx bcy |> sgn in

          (* TODO: decide on whether to allow collinear or not.
             if not, `if turn = 0` should return false rather than loop.

             Might also provide a is_strictly_convex alternative for flexibility. *)
          if turn = 0 then loop (i + 1)
          else if !dir = 0 then (
            dir := turn;
            loop (i + 1))
          else if turn = !dir then loop (i + 1)
          else false
      in
      loop 0
end
