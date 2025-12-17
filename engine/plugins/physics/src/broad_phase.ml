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
  Grid.clear_grid grid;

  if s.len > 0 then
    for row = 0 to s.len - 1 do
      Grid.insert grid row ~min_x:s.min_x.(row) ~min_y:s.min_y.(row) ~max_x:s.max_x.(row)
        ~max_y:s.max_y.(row)
    done;
  grid

let update_potential_collision_pairs c grid =
  (* This shouldn't happen but why not check it anyway *)
  if c.current_generation = Int.max_int then (
    c.current_generation <- 1;
    Array.fill c.pair_generation 0 (Array.length c.pair_generation) 0)
  else c.current_generation <- c.current_generation + 1;
  clear c;

  let occ_len = Dynarray.length Grid.(grid.occupied) in

  for i = 0 to occ_len - 1 do
    let idx = Dynarray.get grid.occupied i in
    let row = idx / grid.cols in
    let col = idx mod grid.cols in
    let current_cell = grid.cells.(idx) in
    let n = current_cell.len in

    (* pairs within the same cell *)
    if n >= 2 then
      for a = 0 to n - 2 do
        for b = a + 1 to n - 1 do
          let id1 = current_cell.data.(a) in
          let id2 = current_cell.data.(b) in
          add_pair c id1 id2
        done
      done;

    (* pairs with neighbors *)
    if n > 0 then
      for dir = 0 to 3 do
        let new_col = col + dx.(dir) in
        let new_row = row + dy.(dir) in

        if new_col >= 0 && new_col < grid.cols && new_row >= 0 && new_row < grid.rows then
          let adjacent_cell = grid.cells.(Grid.cell_index grid ~row:new_row ~col:new_col) in

          if adjacent_cell.len > 0 then
            for a = 0 to current_cell.len - 1 do
              let id1 = current_cell.data.(a) in

              for b = 0 to adjacent_cell.len - 1 do
                let id2 = adjacent_cell.data.(b) in
                add_pair c id1 id2
              done
            done
      done
  done

let pairs_view c = (c.ids1, c.ids2)

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "broad_phase"
end)
