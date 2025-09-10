type t = {
  background_colour : string option;
  class_ : string option;
  compression_level : int;
  infinite : bool;
  layers : Types.layer list;
  next_layer_id : int;
  next_object_id : int;
  orientation : Types.orientation;
  parallax_origin_x : float;
  parallax_origin_y : float;
  properties : string list; (* TODO *)
  render_order : Types.render_order;
  tiled_version : string;
  tile_size : Types.size;
  map_size : Types.map_size;
  tilesets : Types.tileset_outer list;
  path : string;
}

let create
    ?(background_colour = None)
    ?(class_ = None)
    ?(compression_level = -1)
    ?(parallax_origin_x = 0.)
    ?(parallax_origin_y = 0.)
    ?(properties = [])
    ?(render_order = Types.Right_down)
    ~infinite
    ~layers
    ~next_layer_id
    ~next_object_id
    ~orientation
    ~tiled_version
    ~tile_size
    ~map_size
    ~tilesets
    ~path
    () =
  {
    background_colour;
    class_;
    compression_level;
    parallax_origin_x;
    parallax_origin_y;
    properties;
    infinite;
    layers;
    next_layer_id;
    next_object_id;
    orientation;
    render_order;
    tiled_version;
    tile_size;
    map_size;
    tilesets;
    path;
  }

let background_colour map = map.background_colour
let class_ map = map.class_
let compression_level map = map.compression_level
let parallax_origin_x map = map.parallax_origin_x
let parallax_origin_y map = map.parallax_origin_y
let properties map = map.properties
let render_order map = map.render_order
let infinite map = map.infinite
let layers map = map.layers
let next_layer_id map = map.next_layer_id
let next_object_id map = map.next_object_id
let orientation map = map.orientation
let tiled_version map = map.tiled_version
let tile_size map = map.tile_size
let map_size map = map.map_size
let tilesets map = map.tilesets
let path map = map.path
