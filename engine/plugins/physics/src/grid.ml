open Luma__math

module Grid_cell = struct
  type t = {
    mutable len : int;
    mutable cap : int;
    mutable data : int array;
  }

  let create () = { len = 0; cap = 4; data = Array.make 4 0 }
  let clear c = c.len <- 0

  let push c v =
    if c.len = c.cap then (
      let cap = max 4 (2 * c.cap) in
      let data = Array.make cap 0 in
      Array.blit c.data 0 data 0 c.len;
      c.data <- data;
      c.cap <- cap);
    c.data.(c.len) <- v;
    c.len <- c.len + 1
end

type t = {
  cells : Grid_cell.t array;  (** 1d array representing 2D grid. *)
  cell_size : float;
  rows : int;  (** Grid dimensions. *)
  cols : int;  (** Grid dimensions. *)
  world_min : Vec2.t;  (** World boundaries for grid mapping. *)
  world_max : Vec2.t;  (** World boundaries for grid mapping. *)
  occupied : int Dynarray.t;  (** A collection of all the currently occupied cell indexes. *)
  touched : bool array;
      (** A collection of bools indicating whether the corresponding cell in [cells] has been
          touched this frame. *)
}

(** [create world_bound cell_size] *)
let create world_bound cell_size =
  let open Bounded2d in
  let world_min = Aabb2d.min world_bound in
  let world_max = Aabb2d.max world_bound in
  let world_size = Vec2.sub world_max world_min in

  if cell_size > world_size.x || cell_size > world_size.y then
    failwith "cell_size is greater than world size";

  let cols = int_of_float (ceil (world_size.x /. cell_size)) in
  let rows = int_of_float (ceil (world_size.y /. cell_size)) in
  let length = cols * rows in
  let cells = Array.init length (fun _ -> Grid_cell.create ()) in
  let occupied = Dynarray.create () in
  let touched = Array.make length false in

  { cells; cell_size; rows; cols; world_min; world_max; occupied; touched }

(** [cell_index grid row col] converts 2D grid coordinates to 1D array index. *)
let cell_index grid ~row ~col = (row * grid.cols) + col

let clear_grid grid =
  for i = 0 to Dynarray.length grid.occupied - 1 do
    let idx = Dynarray.get grid.occupied i in
    Grid_cell.clear grid.cells.(idx);
    grid.touched.(idx) <- false
  done;
  Dynarray.clear grid.occupied

let grid_cell grid ~pos_x ~pos_y =
  let relative_pos = Vec2.sub (Vec2.create pos_x pos_y) grid.world_min in
  let col = relative_pos.x /. grid.cell_size |> int_of_float in
  let row = relative_pos.y /. grid.cell_size |> int_of_float in

  (* clamp to grid boundaries *)
  let col = max 0 (min col (grid.cols - 1)) in
  let row = max 0 (min row (grid.rows - 1)) in
  (col, row)

(** [insert grid body_index ~min_x ~min_y ~max_x ~max_y] *)
let insert grid body_index ~min_x ~min_y ~max_x ~max_y =
  let open Bounded2d.Aabb2d in
  let start_col, start_row = grid_cell grid ~pos_x:min_x ~pos_y:min_y in
  let end_col, end_row = grid_cell grid ~pos_x:max_x ~pos_y:max_y in

  for row = start_row to end_row do
    for col = start_col to end_col do
      let cell_idx = cell_index grid ~row ~col in
      if not grid.touched.(cell_idx) then (
        grid.touched.(cell_idx) <- true;
        Dynarray.add_last grid.occupied cell_idx);
      Grid_cell.push grid.cells.(cell_idx) body_index
    done
  done

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "physics_grid"
end)
