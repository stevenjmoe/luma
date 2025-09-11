open Types

type t

val create :
  ?background_colour:string option ->
  ?class_:string option ->
  ?compression_level:int ->
  ?parallax_origin_x:float ->
  ?parallax_origin_y:float ->
  ?properties:string list ->
  ?render_order:Types.render_order ->
  infinite:bool ->
  layers:Types.layer list ->
  next_layer_id:int ->
  next_object_id:int ->
  orientation:Types.orientation ->
  tiled_version:string ->
  tile_size:Types.size ->
  map_size:Types.map_size ->
  tilesets:Types.tileset_outer list ->
  path:string ->
  unit ->
  t
(** [create ?background_colour ?class_ ?compression_level ?parallax_origin_x ?parallax_origin_y
     ?properties ?render_order ~infinite ~layers ~next_layer_id ~next_object_id ~orientation
     ~tiled_version ~tile_size ~map_size ~tilesets ~path] *)

val background_colour : t -> string option
val class_ : t -> string option
val compression_level : t -> int
val parallax_origin_x : t -> float
val parallax_origin_y : t -> float
val properties : t -> string list
val render_order : t -> Types.render_order
val infinite : t -> bool
val layers : t -> Types.layer list
val next_layer_id : t -> int
val next_object_id : t -> int
val orientation : t -> Types.orientation
val tiled_version : t -> string
val tile_size : t -> Types.size
val map_size : t -> Types.map_size
val tilesets : t -> Types.tileset_outer list
val path : t -> string
