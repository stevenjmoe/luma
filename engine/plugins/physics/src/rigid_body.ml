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
  mutable pos : Vec2.t;
  mutable vel : Vec2.t;
  mutable acc : Vec2.t;
  mutable force_accumulator : Vec2.t;
  mass : float;
  inv_mass : float;
  damping : float;
  mutable angle : float;
  mutable active : bool;
}

let encode_body_type = function Static -> 0 | Dynamic -> 1 | Kinematic -> 2

let decode_body_type = function
  | 0 -> Static
  | 1 -> Dynamic
  | 2 -> Kinematic
  | other -> failwith (Printf.sprintf "unsupported body type %d" other)

module type S = sig
  module C : Luma__ecs.Component.S with type t = t

  val encode_body_type : body_type -> int
  val decode_body_type : int -> body_type
  val inv_mass : float -> float

  val create_circle : ?mass:float -> body_type -> Vec2.t -> float -> t
  (** [create_circle ?mass body_type pos radius] *)

  val create_box : body_type -> Vec2.t -> Vec2.t -> float -> t
  (** [create_box body_type pos size mass] *)
end

module Make (L : Luma.S) (Config : Config.S) : S = struct
  let encode_body_type = encode_body_type
  let decode_body_type = decode_body_type

  module Velocity = struct
    type t = {
      lin_vel : Vec2.t;
      ang_vel : float;
    }

    let linear lin_vel = { lin_vel; ang_vel = 0. }
    let angular ang_vel = { ang_vel; lin_vel = Vec2.zero }

    module C = L.Component.Make (struct
      type inner = t

      let name = "physics_vel"
    end)
  end

  let inv_mass mass =
    assert (mass >= 0.);
    if mass > 0. then 1. else 0.

  let create_circle ?(mass = 0.) body_type pos radius =
    let circle = Bounded2d.Bounding_circle.create pos radius in
    {
      body_type;
      pos;
      vel = Vec2.zero;
      acc = Vec2.zero;
      mass;
      inv_mass = inv_mass mass;
      damping = 0.99;
      active = true;
      angle = 0.;
      force_accumulator = Vec2.zero;
      shape = Circle circle;
    }

  let create_box body_type pos size mass =
    let inv_mass = inv_mass mass in
    let aabb = Bounded2d.Aabb2d.of_center_halfsize pos (Vec2.scale size 0.5) in
    {
      body_type;
      shape = Aabb aabb;
      pos;
      vel = Vec2.zero;
      acc = Vec2.zero;
      mass;
      inv_mass;
      damping = 0.99;
      active = true;
      angle = 0.;
      force_accumulator = Vec2.zero;
    }

  let moi_of_circle mass radius = 0.5 *. mass *. radius *. radius
  let moi_of_aabb mass (size : Vec2.t) = mass *. ((size.x *. size.x) +. (size.y *. size.y)) /. 12.

  (** [moi body] calculates the moment of inertia. *)
  let moi body =
    match body.shape with
    | Circle c -> moi_of_circle body.mass (Bounded2d.Bounding_circle.radius c)
    | Aabb a -> moi_of_aabb body.mass Bounded2d.Aabb2d.(Vec2.sub (max a) (min a))

  let bounding_box body =
    match body.shape with Circle c -> Bounded2d.Bounding_circle.aabb_2d c | Aabb a -> a

  module C = L.Component.Make (struct
    type inner = t

    let name = "rigid_body"
  end)
end
