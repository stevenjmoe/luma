module type S = sig
  type maps
  type app

  module R : Luma.Resource.S with type t = maps

  type metadata = {
    source : string;
    width : int;
    height : int;
    tile_width : int;
    tile_height : int;
    orientation : Map.orientation;
    background_colour : string option;
  }

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
      loading parsed map data and can be queried. *)

  val tilemaps_loaded : Luma.Ecs.World.t -> bool
  (** [tilemaps_loaded world] returns true if all maps added to the world are queryable. *)

  val tilemap_renderable : Luma.Ecs.World.t -> Luma.Assets.handle -> bool
  (** [tilemap_renderable world handle] returns true if the map has loaded textures and a render
      plan. *)

  val tilemaps_renderable : Luma.Ecs.World.t -> bool
  (** [tilemaps_renderable world] returns true if all maps added to the world are renderable. *)

  val metadata : Luma.Ecs.World.t -> Luma.Assets.handle -> metadata option
  (** [metadata world handle] returns stable map metadata once the map is queryable. *)

  val object_layers :
    Luma.Ecs.World.t -> Luma.Assets.handle -> (string * Object.Object_data.t list) list option
  (** [object_layers world handle] returns all object layers with raw map-space object data once the
      map is queryable. *)

  val object_layer :
    Luma.Ecs.World.t -> Luma.Assets.handle -> string -> Object.Object_data.t list option
  (** [object_layer world handle name] returns the named object layer when the map is queryable. *)

  val find_objects :
    ?layer:string ->
    ?name:string ->
    ?type_:string ->
    Luma.Ecs.World.t ->
    Luma.Assets.handle ->
    Object.Object_data.t list option
  (** [find_objects world handle] returns raw map-space objects matching all provided filters. *)

  val plugin : app -> app
end

module Make : functor (L : Luma.S) -> S with type app = L.App.t
