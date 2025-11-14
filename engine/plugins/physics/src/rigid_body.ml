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

let encode_shape = function Circle _ -> 0 | Aabb _ -> 1

let bounding_box body =
  match body.shape with Circle c -> Bounded2d.Bounding_circle.aabb_2d c | Aabb a -> a

let inv_mass mass =
  assert (mass >= 0.);
  if mass > 0. then 1. else 0.

(** [create_circle ?mass body_type pos radius] *)
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

(** [create_box ?mass body_type pos size] *)
let create_box ?(mass = 1.) body_type pos size =
  let mass, inv_mass = if body_type = Static then (0., 0.) else (mass, inv_mass mass) in
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

module Velocity = struct
  type t = {
    lin_vel : Vec2.t;
    ang_vel : float;
  }

  let linear lin_vel = { lin_vel; ang_vel = 0. }
  let angular ang_vel = { ang_vel; lin_vel = Vec2.zero }

  module C = Luma__ecs.Component.Make (struct
    type inner = t

    let name = "physics_vel"
  end)
end

module C = Luma__ecs.Component.Make (struct
  type inner = t

  let name = "rigid_body"
end)
