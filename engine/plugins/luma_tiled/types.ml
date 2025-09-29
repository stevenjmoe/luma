let flipped_horizontally_flag = 0x8000_0000
let flipped_vertically_flag = 0x4000_0000
let flipped_diagonally_flag = 0x2000_0000

let all_flip_flags =
  flipped_horizontally_flag lor flipped_vertically_flag lor flipped_diagonally_flag

type stagger_axis =
  | X
  | Y

type stagger_index =
  | Even
  | Odd

type orientation =
  | Orthogonal
  | Isometric
  | Staggered
  | Hexagonal

type render_order =
  | Right_down
  | Right_up
  | Left_down
  | Left_up

type fill_mode =
  | Stretch
  | Preserve_aspect_fit

type colour = {
  alpha : int;
  red : int;
  green : int;
  blue : int;
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
  image_width : int;
  image_height : int;
  width : int option;
  height : int option;
  position : position;
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
  width : int;
  height : int;
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
  width : int;
  height : int;
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

type point = {
  x : int;
  y : int;
}

type tiled_object = {
  ellipse : bool option;
  gid : int option;
  height : float;
  width : float;
  id : int;
  name : string;
  point : bool option;
  polygons : point array option;
  polylines : point array option;
  properties : property array option;
  rotation : float;
  template : string option;
  text : string option; (* TODO *)
  visible : bool;
  x : float;
  y : float;
}

type object_group = {
  draw_order : draw_order;
  objects : tiled_object list;
}

type image_layer = {
  image : string;
  width : int;
  height : int;
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
