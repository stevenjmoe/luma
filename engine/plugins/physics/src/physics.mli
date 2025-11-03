module Make (L : Luma.S) : sig
  open Luma__math
  module Config : Config.S
  module Rigid_body : Rigid_body.S

  val get_pos : L.World.t -> L.Id.Entity.t -> Vec2.t option
  val setup : L.App.t -> Config.t option -> L.App.t
  val plugin : ?world_config:Config.t option -> L.App.t -> L.App.t
end
