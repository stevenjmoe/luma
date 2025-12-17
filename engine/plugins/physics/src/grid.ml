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
  occupied : int Dynarray.t;
      (** Cell indices that were written to this frame. Used only for fast clearing between
          rebuilds. *)
  touched : bool array;
      (** A collection of bools indicating whether the corresponding cell in [cells] has been
          touched this frame. *)
  mutable query_generation : int;
      (** The current query generation. Incremented once per grid query. If it would overflow,
          [seen] is cleared and generation restarts at 1. *)
  mutable seen : int array;
      (** Indexed by body_index ([Grid_cell.data.(i)]). Stores the last query generation in which
          this body was seen. Length must be > max body_index encountered; grows as needed (or is
          sized to body store capacity). *)
}

(** [create world_bound cell_size] *)
let create world_bound cell_size =
  let open Bounded2d in
  let world_min = Aabb2d.min world_bound in
  let world_max = Aabb2d.max world_bound in
  let world_size = Vec2.sub world_max world_min in

  if cell_size > world_size.x || cell_size > world_size.y then
    failwith "[Luma.Physics.Grid] cell_size is greater than world size";

  let cols = int_of_float (ceil (world_size.x /. cell_size)) in
  let rows = int_of_float (ceil (world_size.y /. cell_size)) in
  let length = cols * rows in
  let cells = Array.init length (fun _ -> Grid_cell.create ()) in
  let occupied = Dynarray.create () in
  let touched = Array.make length false in
  let seen = Array.make 64 0 in

  {
    cells;
    cell_size;
    rows;
    cols;
    world_min;
    world_max;
    occupied;
    touched;
    query_generation = 1;
    seen;
  }

(** [cell_index grid row col] converts 2D grid coordinates to 1D array index. *)
let cell_index grid ~row ~col = (row * grid.cols) + col

let ensure_seen grid body =
  let need = body + 1 in
  if need > Array.length grid.seen then (
    let new_len = max need (max 64 (2 * Array.length grid.seen)) in
    let a = Array.make new_len 0 in
    Array.blit grid.seen 0 a 0 (Array.length grid.seen);
    grid.seen <- a)

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

(* TODO: query filter *)
let iter_aabb grid ~min_x ~min_y ~max_x ~max_y ~f =
  let start_col, start_row = grid_cell grid ~pos_x:min_x ~pos_y:min_y in
  let end_col, end_row = grid_cell grid ~pos_x:max_x ~pos_y:max_y in

  grid.query_generation <- grid.query_generation + 1;
  if grid.query_generation = max_int then (
    Array.fill grid.seen 0 (Array.length grid.seen) 0;
    grid.query_generation <- grid.query_generation + 1);

  for row = start_row to end_row do
    for col = start_col to end_col do
      let cell_idx = cell_index grid ~row ~col in
      let cell = grid.cells.(cell_idx) in
      for i = 0 to cell.len - 1 do
        let body = cell.data.(i) in
        ensure_seen grid body;
        if grid.seen.(body) <> grid.query_generation then (
          grid.seen.(body) <- grid.query_generation;
          f body)
      done
    done
  done

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "physics_grid"
end)
