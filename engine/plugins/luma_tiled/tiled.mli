module type S = sig
  type maps
  type app

  module R : Luma.Resource.S with type t = maps

  val add :
    Luma.Ecs.World.t ->
    string ->
    Luma.Math.Vec2.t ->
    float ->
    int ->
    maps ->
    (Luma.Assets.handle, Luma__core__Error.error) result
  (** [add world path origin scale z tilemaps] returns [Ok (handle)] if the asset server
      successfully starts loading the map, otherwise an [Error]. *)

  val tilemap_loaded : Luma.Ecs.World.t -> Luma.Assets.handle -> bool
  (** [tilemap_loaded world handle] returns true if the map with the given handle has finished
      loading all assets. *)

  val tilemaps_loaded : Luma.Ecs.World.t -> bool
  (** [tilemaps_loaded world] returns true if all maps added to the world have finished loading all
      assets. *)

  val plugin : app -> app
end

module Make : functor (L : Luma.S) -> S with type app = L.App.t
