module Make (L : Luma.S) : sig
  (** {1 Types} *)

  type t

  module R : L.Resource.S with type t = t

  val add : L.World.t -> string -> Luma__math.Vec2.t -> float -> int -> t -> L.Assets.handle option
  (** [add world path origin scale z tilemaps] returns [Some (handle)] if the asset server
      successfully starts loading the map, otherwise None. *)

  val tilemap_loaded : L.World.t -> L.Assets.handle -> bool
  (** [tilemap_loaded world handle] returns true if the map with the given handle has finished
      loading all assets. *)

  val tilemaps_loaded : L.World.t -> bool
  (** [tilemaps_loaded world] returns true if all maps added to the world have finished loading all
      assets. *)

  val plugin : L.App.t -> L.App.t
end
