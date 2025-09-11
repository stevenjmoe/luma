type tile_offset = {
  x : int;
  y : int;
}

type tile_render_size =
  | Tile
  | Grid

type tile_transformations = {
  hflip : bool;
  vflip : bool;
  rotate : bool;
  prefer_untransformed : bool;
}

type t = {
  background_colour : string option;
  class_ : string option;
  columns : int;
  fill_mode : Types.fill_mode;
  first_gid : int option;
  grid : Types.grid option;
  image : string option;
  image_width : int;
  image_height : int;
  margin : int;
  name : string;
  object_alignment : Types.object_alignment;
  properties : string list; (* TODO *)
  source : string option;
  spacing : int;
  terrains : string list; (* TODO *)
  tile_count : int;
  tiled_version : string;
  tile_width : int;
  tile_height : int;
  tile_offset : tile_offset option;
  tile_render_size : tile_render_size;
  tiles : Types.tile array;
  transformations : tile_transformations option;
  transparent_colour : string option;
  wang_sets : string array; (*TODO*)
}

let background_colour set = set.background_colour
let class_ set = set.class_
let columns set = set.columns
let fill_mode set = set.fill_mode
let first_gid set = set.first_gid
let grid set = set.grid
let image set = set.image
let image_width set = set.image_width
let image_height set = set.image_height
let margin set = set.margin
let name set = set.name
let object_alignment set = set.object_alignment
let properties set = set.properties
let source set = set.source
let spacing set = set.spacing
let terrains set = set.terrains
let tile_count set = set.tile_count
let tiled_version set = set.tiled_version
let tile_width set = set.tile_width
let tile_height set = set.tile_height
let tile_offset set = set.tile_offset
let tile_render_size set = set.tile_render_size
let tiles set = set.tiles
let transformations set = set.transformations
let transparent_colour set = set.transparent_colour
let wang_sets set = set.wang_sets

let create
    ?background_colour
    ?(class_ = None)
    ?(fill_mode = Types.Stretch)
    ?grid
    ?(object_alignment = Types.Unspecified)
    ?(properties = [])
    ?(tile_render_size = Tile)
    ?(transformations = None)
    ?(transparent_colour = None)
    ?(image = None)
    ?(first_gid = None)
    ?(terrains = [])
    ?(source = None)
    ?(tile_offset = None)
    ?(wang_sets = [||])
    ~columns
    ~image_width
    ~image_height
    ~margin
    ~name
    ~spacing
    ~tile_count
    ~tiled_version
    ~tile_width
    ~tile_height
    ~tiles
    () =
  {
    background_colour;
    class_;
    fill_mode;
    grid;
    object_alignment;
    properties;
    transparent_colour;
    tile_render_size;
    transformations;
    columns;
    first_gid;
    image;
    image_width;
    image_height;
    margin;
    name;
    source;
    spacing;
    terrains;
    tile_count;
    tiled_version;
    tile_width;
    tile_height;
    tile_offset;
    tiles;
    wang_sets;
  }

let from_json json = ()
