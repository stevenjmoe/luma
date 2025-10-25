module Types = struct
  type aabb = {
    min : Vec2.t;  (** The minimum bottom-left point of the box *)
    max : Vec2.t;  (** The maximum top-right point of the box *)
  }

  type circle = {
    center : Vec2.t;
    circle : Primitives.Circle.t;
  }
end

module type Bounding_volume = sig
  type translation
  type rotation
  type half_size
  type t

  val center : t -> translation
  val half_size : t -> half_size
  val visible_area : t -> float
  val contains : t -> t -> bool
  val merge : t -> t -> t
  val grow : t -> half_size -> t
  val shrink : t -> half_size -> t
end

module rec Aabb2d : sig
  type t

  include
    Bounding_volume
      with type t := t
       and type translation = Vec2.t
       and type rotation = Rot2.t
       and type half_size = Vec2.t

  val of_center_halfsize : Vec2.t -> Vec2.t -> t
  val of_min_max : Vec2.t -> Vec2.t -> t
  val min : t -> Vec2.t
  val max : t -> Vec2.t
  val intersects_aabb : t -> t -> bool
  val intersects_circle : t -> Bounding_circle.t -> bool

  (* TODO: val scale_around_center : t -> Vec2.t -> t
  val transformed_by : t -> Vec2.t -> Rot2.t -> t
  val transform_by : t -> Vec2.t -> Rot2.t -> t
  val translate_by : t -> Vec2.t -> t
  val rotated_by : t -> Rot2.t -> t
  val rotate_by : t -> Rot2.t -> t*)
  val bounding_circle : t -> Bounding_circle.t
end = struct
  open Types

  type translation = Vec2.t
  type rotation = Rot2.t
  type half_size = Vec2.t
  type t = Types.aabb

  let closest_point aabb point = Vec2.clamp aabb.min aabb.max point

  let of_center_halfsize center half_size =
    assert (Vec2.x half_size >= 0. && Vec2.y half_size >= 0.);
    { min = Vec2.sub center half_size; max = Vec2.add center half_size }

  let of_min_max min max = { min; max }
  let min aabb = aabb.min
  let max aabb = aabb.max
  let center aabb = Vec2.div (Vec2.add aabb.min aabb.max) (Vec2.splat 2.)
  let half_size aabb = Vec2.div (Vec2.sub aabb.max aabb.min) (Vec2.splat 2.)

  let visible_area aabb =
    let b = Vec2.max (Vec2.sub aabb.max aabb.min) Vec2.zero in
    b.x *. b.y

  let contains aabb1 aabb2 =
    aabb2.min.x >= aabb1.min.x
    && aabb2.min.y >= aabb1.min.y
    && aabb2.max.x <= aabb1.max.x
    && aabb2.max.y <= aabb1.max.y

  let merge aabb1 aabb2 = { min = Vec2.min aabb1.min aabb2.min; max = Vec2.max aabb1.max aabb2.max }
  let grow aabb amount = { min = Vec2.sub aabb.min amount; max = Vec2.add aabb.max amount }
  let shrink aabb amount = { min = Vec2.add aabb.min amount; max = Vec2.sub aabb.max amount }

  let bounding_circle aabb =
    let radius = Vec2.distance aabb.min aabb.max /. 2. in
    Bounding_circle.create (center aabb) radius

  let intersects_aabb a b =
    let x_overlaps = a.min.x <= b.max.x && a.max.x >= b.min.x in
    let y_overlaps = a.min.y <= b.max.y && a.max.y >= b.min.y in
    x_overlaps && y_overlaps

  let intersects_circle aabb circle =
    let cp = closest_point aabb circle.center in
    let ds = Vec2.distance_squared circle.center cp in
    let radius_squared = circle.circle.radius *. circle.circle.radius in
    ds <= radius_squared
end

and Bounding_circle : sig
  type t

  include
    Bounding_volume
      with type t := t
       and type translation = Vec2.t
       and type rotation = Rot2.t
       and type half_size = float

  val create : Vec2.t -> float -> t
  val center : t -> Vec2.t
  val radius : t -> float
  val aabb_2d : t -> Aabb2d.t
  val intersects_aabb : t -> Aabb2d.t -> bool
  val intersects_circle : t -> t -> bool
end = struct
  open Types

  type translation = Vec2.t
  type rotation = Rot2.t
  type half_size = float
  type t = circle

  let create center radius =
    assert (radius >= 0.);
    { center; circle = Primitives.Circle.create radius }

  let center b = b.center
  let radius b = b.circle.radius
  let half_size c = radius c
  let visible_area c = Float.pi *. c.circle.radius *. c.circle.radius
  let contains c1 c2 = failwith "TODO"
  let merge c1 c2 = failwith "TODO"
  let grow c1 c2 = failwith "TODO"
  let shrink c1 c2 = failwith "TODO"

  let aabb_2d b =
    let min = Vec2.sub b.center (Vec2.splat b.circle.radius) in
    let max = Vec2.add b.center (Vec2.splat b.circle.radius) in
    Aabb2d.of_min_max min max

  let intersects_aabb circle aabb = Aabb2d.intersects_circle aabb circle

  let intersects_circle circle1 circle2 =
    let center_distance_squared = Vec2.distance_squared circle1.center circle2.center in
    let radius_sum = circle1.circle.radius +. circle2.circle.radius in
    let radius_sum_squared = radius_sum *. radius_sum in
    center_distance_squared <= radius_sum_squared
end
