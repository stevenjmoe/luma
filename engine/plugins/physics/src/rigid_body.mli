open Luma__math

type body_type =
  | Static
  | Dynamic
  | Kinematic

type shape =
  | Circle of Bounded2d.Bounding_circle.t
  | Aabb of Bounded2d.Aabb2d.t

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
val bounding_box : t -> Bounded2d.Aabb2d.t
val compute_inv_mass : float -> float
val create_circle : ?mass:float -> body_type -> Vec2.t -> float -> t
val create_box : ?mass:float -> body_type -> Vec2.t -> Vec2.t -> t
val moi_of_circle : float -> float -> float
val moi_of_aabb : float -> Vec2.t -> float
val moi : t -> float

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
