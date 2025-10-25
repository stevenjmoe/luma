module rec Aabb2d : sig
  type t

  val of_center_halfsize : Vec2.t -> Vec2.t -> t
  (** [of_min_max center half_size] Constructs an AABB from its center and half-size. *)

  val of_min_max : Vec2.t -> Vec2.t -> t
  (** [of_min_max min max] Constructs an AABB from its min and max. *)

  val min : t -> Vec2.t
  val max : t -> Vec2.t
  val center : t -> Vec2.t
  val bounding_circle : t -> Bounding_circle.t
  val half_size : t -> Vec2.t
  val visible_area : t -> float
  val contains : t -> t -> bool
  val merge : t -> t -> t
  val grow : t -> Vec2.t -> t
  val shrink : t -> Vec2.t -> t
end

and Bounding_circle : sig
  type t

  val create : Vec2.t -> float -> t
  val center : t -> Vec2.t
  val radius : t -> float

  val aabb_2d : t -> Aabb2d.t
  (** Computes the smallest [Aabb2d.t] containing this [Bounding_circle.t]. *)
end
