open Luma__math
open Containers

type t = {
  size : Vec2.t;
  textures : (Rect.t, Vector.rw) Vector.t;
  frame_size : Vec2.t option;
  mutable last_index : int option;
}

let empty () =
  { size = Vec2.zero; textures = Vector.create (); frame_size = None; last_index = None }

let size t = t.size
let textures t = t.textures
let frame_size t = t.frame_size
let last_index t = t.last_index

let from_grid ?(padding = Vec2.zero) ?(offset = Vec2.zero) tile_size columns rows =
  let open Vec2.Infix in
  let sprites = Vector.create () in
  let current_padding = Vec2.zero in
  let foi = Float.of_int in
  let index = ref 0 in

  for y = 1 to rows do
    if y > 1 then current_padding.y <- padding.y;

    for x = 1 to columns do
      if x > 1 then current_padding.x <- padding.x;

      let cell = Vec2.create (foi x) (foi y) in
      let pos = (tile_size *.. cell) +.. offset in
      let rect = Rect.create ~pos ~size:tile_size in
      Vector.push sprites rect;
      incr index
    done
  done;

  let last_index = if !index = 0 then None else Some (!index - 1) in
  let grid_size = Vec2.create (foi columns) (foi rows) in

  {
    size = ((tile_size +.. current_padding) *.. grid_size) -.. current_padding;
    textures = sprites;
    frame_size = Some tile_size;
    last_index;
  }

let length t = Vector.length t.textures
let is_empty t = Vector.length t.textures = 0

let get_frame t index =
  if index >= 0 && index < Vector.length t.textures then Some (Vector.get t.textures index)
  else None

let append_texture texture t =
  Vector.push t.textures texture;
  let last_index = Vector.length t.textures - 1 in
  { t with last_index = Some last_index }

let insert_texture texture idx t =
  if idx < 0 || idx > Vector.length t.textures then
    invalid_arg "insert_texture: index out of bounds";

  Vector.insert t.textures idx texture;
  let last_index = Vector.length t.textures - 1 in
  { t with last_index = Some last_index }
