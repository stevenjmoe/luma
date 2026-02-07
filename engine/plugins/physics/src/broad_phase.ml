type t = {
  max_bodies : int;
  mutable current_generation : int;
  pair_generation : int array;
  ids1 : int Dynarray.t;
  ids2 : int Dynarray.t;
}

let dx = [| 1; 1; 0; -1 |]
let dy = [| 0; 1; 1; 1 |]

let create ~max_bodies ?(pairs_cap = 0) () =
  let ids1 = Dynarray.create () in
  let ids2 = Dynarray.create () in
  if pairs_cap > 0 then (
    Dynarray.ensure_capacity ids1 pairs_cap;
    Dynarray.ensure_capacity ids2 pairs_cap);

  let pair_generation = Array.make (max_bodies * max_bodies) 0 in

  { max_bodies; pair_generation; current_generation = 0; ids1; ids2 }

let clear c =
  Dynarray.clear c.ids1;
  Dynarray.clear c.ids2

let pair_index ~max_bodies ~id1 ~id2 =
  assert (id1 < max_bodies && id2 < max_bodies);
  let a = min id1 id2 in
  let b = max id1 id2 in
  (a * max_bodies) + b

let add_pair c id1 id2 =
  let a = min id1 id2 in
  let b = max id1 id2 in
  if a <> b then
    let idx = pair_index ~max_bodies:c.max_bodies ~id1:a ~id2:b in
    if c.pair_generation.(idx) <> c.current_generation then (
      c.pair_generation.(idx) <- c.current_generation;
      Dynarray.add_last c.ids1 id1;
      Dynarray.add_last c.ids2 id2)

let update_broad_phase (s : Rb_store.t) (grid : Grid.t) =
  Grid.clear grid;
  let len = Rb_store.len s in

  if len > 0 then
    for row = 0 to len - 1 do
      let min_x = Rb_store.min_x s row in
      let max_x = Rb_store.max_x s row in
      let min_y = Rb_store.min_y s row in
      let max_y = Rb_store.max_y s row in
      Grid.insert grid row ~min_x ~min_y ~max_x ~max_y
    done

let update_potential_collision_pairs c grid =
  (* This shouldn't happen but why not check it anyway *)
  if c.current_generation = Int.max_int then (
    c.current_generation <- 1;
    Array.fill c.pair_generation 0 (Array.length c.pair_generation) 0)
  else c.current_generation <- c.current_generation + 1;
  clear c;

  let occupied = Grid.occupied grid in
  let occ_len = Dynarray.length occupied in
  let grid_cols = Grid.cols grid in
  let grid_rows = Grid.rows grid in

  for i = 0 to occ_len - 1 do
    let idx = Dynarray.get occupied i in
    let row = idx / grid_cols in
    let col = idx mod grid_cols in
    let current_cell = Grid.cell_at grid idx in
    let current_cell_len = Grid.Grid_cell.len current_cell in

    (* pairs within the same cell *)
    if current_cell_len >= 2 then
      for a = 0 to current_cell_len - 2 do
        for b = a + 1 to current_cell_len - 1 do
          let id1 = Grid.Grid_cell.data_at current_cell a in
          let id2 = Grid.Grid_cell.data_at current_cell b in
          add_pair c id1 id2
        done
      done;

    (* pairs with neighbors *)
    if current_cell_len > 0 then
      for dir = 0 to 3 do
        let new_col = col + dx.(dir) in
        let new_row = row + dy.(dir) in

        if new_col >= 0 && new_col < grid_cols && new_row >= 0 && new_row < grid_rows then
          let adjacent_cell = Grid.cell_at grid (Grid.cell_index grid ~row:new_row ~col:new_col) in
          let adjacent_cell_len = Grid.Grid_cell.len adjacent_cell in

          if Grid.Grid_cell.len adjacent_cell > 0 then
            for a = 0 to current_cell_len - 1 do
              let id1 = Grid.Grid_cell.data_at current_cell a in

              for b = 0 to adjacent_cell_len - 1 do
                let id2 = Grid.Grid_cell.data_at adjacent_cell b in
                add_pair c id1 id2
              done
            done
      done
  done

let step rb_store grid c =
  update_broad_phase rb_store grid;
  update_potential_collision_pairs c grid

let pairs_view c = (c.ids1, c.ids2)

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "broad_phase"
end)
