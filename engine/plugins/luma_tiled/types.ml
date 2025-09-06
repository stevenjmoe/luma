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

type tileset = {
  name : string;
  size : size;
  count : int;
  columns : int;
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
