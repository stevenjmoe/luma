(** The [Rigid_body] component acts as a config and snapshot of a rigid body in the physics world.

    Spawning an entity with [Rigid_body.C] will initialise the body, and querying for it will return
    a snapshot of it's current position and velocity.

    However, despite [Vec2.t] having mutable x and y fields, the [Rigid_body] component itself is
    not mutable; directly mutating the component's position or velocity will not affect the
    underlying store. All changes must go through the [Physics] API. *)

open Luma__math

type body_type =
  | Static
  | Dynamic
  | Kinematic

type shape =
  | Circle of float
  | Aabb of Vec2.t
  | Polygon of Vec2.t array

type polygon_create_error =
  | Needs_at_least_3_points
  | Non_convex_polygon

type t = {
  body_type : body_type;
  shape : shape;
  pos : Vec2.t;
  vel : Vec2.t;
  acc : Vec2.t;
  force_accumulator : Vec2.t;
  mass : float;
  inv_mass : float;
  damping : float;
  angle : float;
  active : bool;
}

val encode_body_type : body_type -> int
val decode_body_type : int -> body_type
val body_type_to_string : body_type -> string
val body_type_of_string : string -> body_type
val encode_shape : shape -> int
val isometry : t -> Isometry.t
val compute_inv_mass : float -> float

val create_circle : ?mass:float -> body_type -> Vec2.t -> float -> t
(** [create_circle ?mass body_type pos radius] *)

val create_box : ?mass:float -> body_type -> Vec2.t -> Vec2.t -> t
(** [create_box ?mass body_type pos size] *)

val create_polygon : ?mass:float -> body_type -> Vec2.t -> Vec2.t array -> (t, polygon_create_error) result
(** [create_polygon ?mass body_type pos points] validates polygon input and returns [Error] on invalid
    shape. *)

val create_polygon_exn : ?mass:float -> body_type -> Vec2.t -> Vec2.t array -> t
(** [create_polygon_exn ?mass body_type pos points] is the exception-raising wrapper over
    [create_polygon]. *)

val moi_of_circle : float -> float -> float
val moi_of_aabb : float -> Vec2.t -> float

module Velocity : sig
  type t = {
    lin_vel : Vec2.t;
    ang_vel : float;
  }

  val linear : Vec2.t -> t
  val angular : float -> t

  module C : Luma__ecs.Component.S with type t = t
end

module C : Luma__ecs.Component.S with type t = t
