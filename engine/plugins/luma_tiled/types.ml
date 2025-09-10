type orientation =
  | Orthogonal
  | Isometric

type render_order =
  | Right_down
  | Right_up
  | Left_down
  | Left_up

type size = {
  w : int;
  h : int;
}

type grid = {
  height : int;
  orientation : orientation;
  width : int;
}

type position = {
  x : int;
  y : int;
}

type tile = {
  id : int;
  image_path : string;
  image_size : size;
  size : size;
  position : position;
}

type tileset = {
  class_ : string option;
  columns : int;
  grid : grid option;
  image : string option;
  image_size : size option;
  margin : int;
  name : string;
  spacing : int;
  tile_count : int;
  tile_size : size;
  tiles : tile array;
}

type map_size =
  | Fixed of {
      rows : int;
      columns : int;
    }
  | Infinite

type tileset_outer = {
  first_gid : int;
  source : string;
}

type layer_data =
  | Array_ of int list
  | String_ of string

type draw_order =
  | Topdown
  | Index

type encoding =
  | Csv
  | Base64

type chunk = {
  start_x : int;
  start_y : int;
  size : size;
  data : layer_data;
}

type property = (* TODO *) Yojson.Safe.t

type layer_common = {
  class_ : string option;
  id : int;
  name : string;
  opacity : float;
  visible : bool;
  offset_x : float;
  offset_y : float;
  parallax_x : float;
  parallax_y : float;
  properties : property list; (* TODO: proper typed props *)
  start_x : int option;
  start_y : int option;
  tint_colour : string option; (* hex *)
  x : int;
  y : int;
}

type compression =
  | Zlib
  | Gzip
  | Zstd
  | None

type tile_layer = {
  size : size;
  encoding : encoding;
  compression : compression;
  data : layer_data;
  chunks : chunk list option;
}

type tileset_object = {
  elipse : bool;
  gid : int;
  height : int;
  id : int;
  name : string;
  rotation : int;
  type_ : string option;
  visible : bool;
  width : int;
  x : float;
  y : float;
}

type object_group = {
  draw_order : string;
  objects : Yojson.Safe.t list; (* TODO: type this *)
}

type image_layer = {
  image : string;
  size : size;
  repeat_x : bool;
  repeat_y : bool;
}

type group = { layers : layer list }

and layer_payload =
  | Tile_layer of tile_layer
  | Object_group of object_group
  | Image of image_layer
  | Group of group

and layer = {
  common : layer_common;
  payload : layer_payload;
}
