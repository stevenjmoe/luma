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

type tile_layer = {
  id : int;
  name : string;
  size : size;
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

type t = {
  orientation : orientation;
  render_order : render_order;
  tile_size : size;
  map_size : map_size;
}
