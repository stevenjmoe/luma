module rec Aabb2d : sig
  type t

  val of_center_halfsize : Vec2.t -> Vec2.t -> t
  (** [of_min_max center half_size] Constructs an AABB from its center and half-size. *)

  val of_min_max : Vec2.t -> Vec2.t -> t
  (** [of_min_max min max] Constructs an AABB from its min and max. *)

  val of_point_cloud : Isometry.t -> Vec2.t array -> t
  (** [of_point_cloud isomentry points] *)

  val min : t -> Vec2.t
  val max : t -> Vec2.t
  val area : t -> float
  val center : t -> Vec2.t
  val bounding_circle : t -> Bounding_circle.t
  val half_size : t -> Vec2.t
  val visible_area : t -> float
  val intersects_aabb : t -> t -> bool
  val intersects_circle : t -> Bounding_circle.t -> bool
  val set_min : t -> Vec2.t -> unit
  val set_max : t -> Vec2.t -> unit
  val contains : t -> t -> bool
  val merge : t -> t -> t
  val grow : t -> Vec2.t -> t
  val shrink : t -> Vec2.t -> t
end

and Bounding_circle : sig
  type t

  val create : Vec2.t -> float -> t
  (** [create center radius] *)

  val of_point_cloud : Isometry.t -> Vec2.t array -> t
  (** [of_point_cloud isometry points] *)

  val center : t -> Vec2.t
  val radius : t -> float

  val aabb_2d : t -> Aabb2d.t
  (** Computes the smallest [Aabb2d.t] containing this [Bounding_circle.t]. *)

  val intersects_aabb : t -> Aabb2d.t -> bool
  val intersects_circle : t -> t -> bool
end

module type Bounded2d = sig
  type t

  val aabb_2d : t -> Isometry.t -> Aabb2d.t
  val bounding_circle : t -> Isometry.t -> Bounding_circle.t
end

module Bounded_polygon : Bounded2d with type t = Vec2.t array
