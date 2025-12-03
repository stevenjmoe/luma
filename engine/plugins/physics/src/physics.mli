module type S = sig
  open Luma__math
  module Rigid_body = Rigid_body

  module Colliders : sig
    type t = Rigid_body.t list

    module C : Luma__ecs.Component.S with type t = Rigid_body.t list
  end

  type app

  val setup : app -> Config.t -> app
  val pos : Luma__ecs.World.t -> Luma__id.Id.EntitySet.elt -> Vec2.t option
  val plugin : ?world_config:Config.t -> app -> app
end

module Make (L : Luma.S) : S with type app = L.App.t
