module Make (L : Luma.S) : sig
  open Luma

  type maps

  module R : Luma.Resource.S with type t = maps

  val add :
    Ecs.World.t ->
    string ->
    Luma__math.Vec2.t ->
    float ->
    int ->
    maps ->
    (L.Assets.handle, Luma__core.Error.error) result
  (** [add world path origin scale z tilemaps] returns [Ok (handle)] if the asset server
      successfully starts loading the map, otherwise an [Error]. *)

  val tilemap_loaded : Ecs.World.t -> L.Assets.handle -> bool
  (** [tilemap_loaded world handle] returns true if the map with the given handle has finished
      loading all assets. *)

  val tilemaps_loaded : Luma.Ecs.World.t -> bool
  (** [tilemaps_loaded world] returns true if all maps added to the world have finished loading all
      assets. *)

  val plugin : L.App.t -> L.App.t
end
