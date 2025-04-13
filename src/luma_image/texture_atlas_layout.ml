open Luma__math
open Containers

type t = {
  size : Vec2.t;
  textures : (Rect.t, Vector.rw) Vector.t;
  frame_size : Vec2.t option;
}

let empty () = { size = Vec2.zero (); textures = Vector.create (); frame_size = None }
let size t = t.size
let textures t = t.textures
let frame_size t = t.frame_size

let from_grid ?(padding = Vec2.zero ()) ?(offset = Vec2.zero ()) tile_size columns rows =
  let open Vec2.Infix in
  let sprites = Vector.create () in
  let current_padding = Vec2.zero () in
  let foi = Float.of_int in

  let rec loop_rows y =
    if y > 1 then
      current_padding.y <- padding.y;

    let rec loop_cols x =
      if x > 1 then
        current_padding.x <- padding.x;

      let cell = Vec2.create (foi x) (foi y) in

      let rect_min = (tile_size *.. cell) +.. offset in
      Vector.push sprites Rect.{ min = rect_min; max = rect_min +.. tile_size };

      if x < columns then
        loop_cols (x + 1)
      else
        ()
    in
    loop_cols 1;

    if y < rows then
      loop_rows (y + 1)
    else
      ()
  in

  loop_rows 1;
  let grid_size = Vec2.create (foi columns) (foi rows) in
  {
    size = ((tile_size +.. current_padding) *.. grid_size) -.. current_padding;
    textures = sprites;
    frame_size = Some tile_size;
  }

let length t = Vector.length t.textures
let is_empty t = Vector.length t.textures = 0

let get_frame t index =
  if index >= 0 && index < Vector.length t.textures then
    Some (Vector.get t.textures index)
  else
    None

let add_texture t texture =
  Vector.push t.textures texture;
  Vector.length t.textures - 1
