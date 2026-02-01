module Types = struct
  type aabb = {
    mutable min : Vec2.t;  (** The minimum bottom-left point of the box *)
    mutable max : Vec2.t;  (** The maximum top-right point of the box *)
  }

  type circle = Primitives.Circle.t
  type capsule = Primitives.Capsule2d.t
end

module type Bounding_volume = sig
  type translation
  type rotation
  type half_size
  type t

  val center : t -> translation
  val half_size : t -> half_size
  val visible_area : t -> float
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
  val area : t -> float
  val intersects_aabb : t -> t -> bool
  val intersects_circle : t -> Bounding_circle.t -> bool
  val set_min : t -> Vec2.t -> unit
  val set_max : t -> Vec2.t -> unit
  val contains : t -> t -> bool
  val merge : t -> t -> t
  val grow : t -> Vec2.t -> t
  val shrink : t -> Vec2.t -> t

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

  let of_point_cloud points (isometry : Isometry.t) =
    let n = Array.length points in
    if n = 0 then failwith "Point cloud must contain at least one point for Aabb2d construction."
    else
      let rot p = Rot2.rotate_vec isometry.rotation p in
      let first = rot points.(0) in
      let min = ref first in
      let max = ref first in

      for i = 1 to n - 1 do
        let rp = rot points.(i) in
        min := Vec2.min !min rp;
        max := Vec2.max !max rp
      done;

      { min = Vec2.add !min isometry.translation; max = Vec2.add !max isometry.translation }

  let min aabb = aabb.min
  let max aabb = aabb.max
  let center aabb = Vec2.div (Vec2.add aabb.min aabb.max) (Vec2.splat 2.)
  let half_size aabb = Vec2.div (Vec2.sub aabb.max aabb.min) (Vec2.splat 2.)

  let area aabb =
    let size = Vec2.sub aabb.max aabb.min in
    size.x *. size.y

  let set_min aabb min = aabb.min <- min
  let set_max aabb max = aabb.max <- max

  let visible_area aabb =
    Aabb2d_raw.visible_area ~min_x:aabb.min.x ~max_x:aabb.max.x ~min_y:aabb.min.y ~max_y:aabb.max.y

  let contains aabb1 aabb2 =
    Aabb2d_raw.contains ~min_x1:aabb1.min.x ~max_x1:aabb1.max.x ~min_y1:aabb1.min.y
      ~max_y1:aabb1.max.y ~min_x2:aabb2.min.x ~max_x2:aabb2.max.x ~min_y2:aabb2.min.y
      ~max_y2:aabb2.max.y

  let merge aabb1 aabb2 = { min = Vec2.min aabb1.min aabb2.min; max = Vec2.max aabb1.max aabb2.max }
  let grow aabb amount = { min = Vec2.sub aabb.min amount; max = Vec2.add aabb.max amount }
  let shrink aabb amount = { min = Vec2.add aabb.min amount; max = Vec2.sub aabb.max amount }

  let bounding_circle aabb =
    let radius = Vec2.distance aabb.min aabb.max /. 2. in
    Bounding_circle.create (center aabb) radius

  let intersects_aabb a b =
    Aabb2d_raw.aabb_intersects_aabb ~a_min_x:a.min.x ~a_max_x:a.max.x ~a_min_y:a.min.y
      ~a_max_y:a.max.y ~b_min_x:b.min.x ~b_max_x:b.max.x ~b_min_y:b.min.y ~b_max_y:b.max.y

  let intersects_circle aabb (circle : Bounding_circle.t) =
    let center = Bounding_circle.center circle in
    let radius = Bounding_circle.radius circle in
    let cp = closest_point aabb center in
    let ds = Vec2.distance_squared center cp in
    let radius_squared = radius *. radius in
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
  val set_center : t -> Vec2.t -> unit
end = struct
  open Types
  open Primitives.Circle

  type translation = Vec2.t
  type rotation = Rot2.t
  type half_size = float

  type t = {
    circle : circle;
    mutable center : Vec2.t;
  }

  let create center radius =
    assert (radius >= 0.);
    { circle = Primitives.Circle.create ~radius; center }

  let center b = b.center
  let radius b = b.circle.radius
  let half_size c = radius c
  let visible_area c = Float.pi *. c.circle.radius *. c.circle.radius
  let set_center c center = c.center <- center

  let aabb_2d b =
    let min = Vec2.sub b.center (Vec2.splat b.circle.radius) in
    let max = Vec2.add b.center (Vec2.splat b.circle.radius) in
    Aabb2d.of_min_max min max

  let intersects_aabb circle aabb = Aabb2d.intersects_circle aabb circle

  let intersects_circle circle1 circle2 =
    Aabb2d_raw.circle_intersects_circle ~a_center_x:circle1.center.x ~a_center_y:circle1.center.y
      ~a_radius:circle1.circle.radius ~b_center_x:circle2.center.x ~b_center_y:circle2.center.y
      ~b_radius:circle2.circle.radius
end

module type Boudned2d = sig
  type shape

  val aabb2d : shape -> Isometry.t -> Aabb2d.t
  val bounding_circle : shape -> Isometry.t -> Bounding_circle.t
end
