type tile_offset = {
  x : int;
  y : int;
}

type tile_transformations = {
  hflip : bool;
  vflip : bool;
  rotate : bool;
  prefer_untransformed : bool;
}

type tile_render_size =
  | Tile
  | Grid

type t

val create :
  ?background_colour:string ->
  ?class_:string option ->
  ?fill_mode:Types.fill_mode ->
  ?grid:Types.grid ->
  ?object_alignment:Types.object_alignment ->
  ?properties:string list ->
  ?tile_render_size:tile_render_size ->
  ?transformations:tile_transformations option ->
  ?transparent_colour:string option ->
  ?image:string option ->
  ?first_gid:int option ->
  ?terrains:string list ->
  ?source:string option ->
  ?tile_offset:tile_offset option ->
  ?wang_sets:string array ->
  columns:int ->
  image_width:int ->
  image_height:int ->
  margin:int ->
  name:string ->
  spacing:int ->
  tile_count:int ->
  tiled_version:string ->
  tile_width:int ->
  tile_height:int ->
  tiles:Types.tile array ->
  unit ->
  t

val image : t -> string option
val background_colour : t -> string option
val class_ : t -> string option
val columns : t -> int
val fill_mode : t -> Types.fill_mode
val first_gid : t -> int option
val grid : t -> Types.grid option
val image : t -> string option
val image_width : t -> int
val image_height : t -> int
val margin : t -> int
val name : t -> string
val object_alignment : t -> Types.object_alignment
val properties : t -> string list
val source : t -> string option
val spacing : t -> int
val terrains : t -> string list
val tile_count : t -> int
val tiled_version : t -> string
val tile_width : t -> int
val tile_height : t -> int
val tile_offset : t -> tile_offset option
val tile_render_size : t -> tile_render_size
val tiles : t -> Types.tile array
val transformations : t -> tile_transformations option
val transparent_colour : t -> string option
val wang_sets : t -> string array
