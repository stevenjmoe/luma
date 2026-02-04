open Luma__math

type body_type =
  | Static
  | Dynamic
  | Kinematic

type shape =
  | Circle of float (* radius *)
  | Aabb of Vec2.t (* half_size *)

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

let encode_body_type = function Static -> 0 | Dynamic -> 1 | Kinematic -> 2

let decode_body_type = function
  | 0 -> Static
  | 1 -> Dynamic
  | 2 -> Kinematic
  | other -> failwith (Printf.sprintf "unsupported body type %d" other)

let body_type_to_string = function
  | Static -> "Static"
  | Dynamic -> "Dynamic"
  | Kinematic -> "Kinematic"

let body_type_of_string = function
  | "Static" | "static" -> Static
  | "Dynamic" | "dynamic" -> Dynamic
  | "Kinematic" | "kinematic" -> Kinematic
  | other -> failwith (Printf.sprintf "unsupported body type %s" other)

let encode_shape = function Circle _ -> 0 | Aabb _ -> 1

let compute_inv_mass mass =
  assert (mass >= 0.);
  if mass > 0. then 1. else 0.

let create_circle ?(mass = 0.) body_type pos radius =
  let mass, inv_mass =
    match body_type with Static | Kinematic -> (0., 0.) | Dynamic -> (mass, compute_inv_mass mass)
  in
  {
    body_type;
    pos;
    vel = Vec2.zero;
    acc = Vec2.zero;
    mass;
    inv_mass;
    damping = 0.99;
    active = true;
    angle = 0.;
    force_accumulator = Vec2.zero;
    shape = Circle radius;
  }

let create_box ?(mass = 1.) body_type pos size =
  let mass, inv_mass =
    match body_type with Static | Kinematic -> (0., 0.) | Dynamic -> (mass, compute_inv_mass mass)
  in
  {
    body_type;
    shape = Aabb (Vec2.scale size 0.5);
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
