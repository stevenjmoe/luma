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

type tile = {
  id : int;
  x : int;
  y : int;
  image_path : string;
  image_size : size;
  size : size;
}

type tileset = {
  name : string;
  grid : grid;
  margin : int;
  spacing : int;
  size : size;
  count : int;
  columns : int;
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
