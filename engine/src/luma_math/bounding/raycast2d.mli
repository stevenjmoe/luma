type t = {
  ray : Ray.Ray2d.t;
  max : float;  (** The maximum distance for the ray *)
  direction_recip : Vec2.t;  (** The multiplicative inverse direction of the ray *)
}

val from_ray : Ray.Ray2d.t -> float -> t
(** [from_ray ray distance] makes a [Raycast2d.t] from a ray and max distance. *)

val create : Vec2.t -> Direction.Dir2.t -> float -> t
(** [create origin direction distance] created a [Raycast2d.t] from an origin, [Dir2.t], and max
    distance. *)

val aabb_intersection_at : t -> Bounded2d.Aabb2d.t -> float option
(** [aabb_intersection_at raycast aabb] gets the distance of an intersection with an [Aabb2d.t], if
    any. *)

val circle_intersection_at : t -> Bounded2d.Bounding_circle.t -> float option
(** [circle_intersection_at raycast circle] gets the distance of an intersection with a
    [Bounding_circle.t], if any. *)

val intersects_aabb : t -> Bounded2d.Aabb2d.t -> bool
val intersects_circle : t -> Bounded2d.Bounding_circle.t -> bool
