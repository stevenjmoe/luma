module Make (L : Luma.S) : sig
  open Luma__math
  open L

  val pos : World.t -> Id.Entity.t -> Vec2.t option
  val setup : App.t -> Config.t -> App.t
  val plugin : ?world_config:Config.t option -> App.t -> App.t
end
