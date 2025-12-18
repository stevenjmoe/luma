module type S = sig
  open Luma__math
  open Luma__ecs
  open Luma__id
  module Rigid_body = Rigid_body
  module Colliders = Colliders

  val pos : Luma__ecs.World.t -> Id.Entity.t -> Vec2.t option

  val move_and_collide :
    World.t -> Id.Entity.t -> velocity:Vec2.t -> dt:float -> (Id.Entity.t * Vec2.t * float) option

  val move_and_slide : ?max_iterations:int -> World.t -> Id.Entity.t -> Vec2.t -> float -> bool
  val plugin : ?world_config:Config.t -> Luma__app.App.t -> Luma__app.App.t
end

module Make (L : Luma.S) : S
