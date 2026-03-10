open Luma__asset
open Luma__math
open Luma__core

let flipped_horizontally_flag = 0x8000_0000l
let flipped_vertically_flag = 0x4000_0000l
let flipped_diagonally_flag = 0x2000_0000l

let all_flip_flags =
  let open Int32 in
  logor flipped_horizontally_flag (logor flipped_vertically_flag flipped_diagonally_flag)

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

type colour = {
  alpha : int;
  red : int;
  green : int;
  blue : int;
}

type image = {
  source : string;
  width : int;
  height : int;
  transparent_colour : colour option;
}

type object_tile_data = {
  handle : Assets.handle;  (** Handle for the object texture. *)
  size : Vec2.t;  (** The width and height of the sub-rectangle. *)
  pos : Vec2.t;  (** The position of the sub-rectangle. *)
}

type tileset_loaded =
  | Texture of {
      texture : Assets.handle;
      cell_w : float;
      cell_h : float;
      columns : int;
      spacing : int;
      margin : int;
    }
  | Textures of { texture_by_tile_id : (int, object_tile_data) Hashtbl.t }

type tileset_texture =
  | Image of Assets.handle
  | Collection_of_images of (int, object_tile_data) Hashtbl.t

type 'plan phase =
  | Init
  | Loading_tilesets of { tileset_handles_by_gid : (int * Assets.handle) list }
  | Loading_textures of {
      tileset_handles_by_gid : (int * Assets.handle) list;
      textures_by_tileset : (int, tileset_texture) Hashtbl.t;
    }
  | Ready of {
      tilesets : (int, tileset_loaded) Hashtbl.t;
      plan : 'plan;
    }
  | Failed of Error.error
