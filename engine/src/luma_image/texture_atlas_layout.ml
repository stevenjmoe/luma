open Luma__math
open Containers

type t = {
  size : Vec2.t;
  textures : (Rect.t, Vector.rw) Vector.t;
  mutable last_index : int option;
}

let empty () = { size = Vec2.zero; textures = Vector.create (); last_index = None }
let size t = t.size
let textures t = t.textures
let last_index t = t.last_index

let from_grid ?(padding = Vec2.zero) ?(offset = Vec2.zero) tile_size columns rows =
  let open Vec2.Infix in
  let sprites = Vector.create () in

  for y = 0 to rows - 1 do
    for x = 0 to columns - 1 do
      let pos =
        (tile_size *.. Vec2.create (float x) (float y))
        +.. offset
        +.. (padding *.. Vec2.create (float x) (float y))
      in
      Vector.push sprites (Rect.create ~pos ~size:tile_size)
    done
  done;

  {
    size =
      (tile_size *.. Vec2.create (float columns) (float rows))
      +.. (padding *.. Vec2.create (float (columns - 1)) (float (rows - 1)));
    textures = sprites;
    last_index = (if rows * columns = 0 then None else Some ((rows * columns) - 1));
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
