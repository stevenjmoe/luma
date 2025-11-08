type t = {
  added_pairs : (int64, unit) Hashtbl.t;
  ids1 : int Dynarray.t;
  ids2 : int Dynarray.t;
}

let dx = [| 1; 1; 0; -1 |]
let dy = [| 0; 1; 1; 1 |]

let create ?(pairs_cap = 0) ?(set_cap = 16384) () =
  let ids1 = Dynarray.create () in
  let ids2 = Dynarray.create () in
  if pairs_cap > 0 then (
    Dynarray.ensure_capacity ids1 pairs_cap;
    Dynarray.ensure_capacity ids2 pairs_cap);

  { added_pairs = Hashtbl.create set_cap; ids1; ids2 }

let clear c =
  Hashtbl.clear c.added_pairs;
  Dynarray.clear c.ids1;
  Dynarray.clear c.ids2

let update_potential_collision_pairs c grid =
  clear c;
  let { added_pairs; ids1; ids2 } = c in

  let pair_key_of_pairs id1 id2 =
    let a = Int64.of_int (min id1 id2) and b = Int64.of_int (max id1 id2) in
    Int64.logor (Int64.shift_left a 32) b
  in

  for row = 0 to Grid.(grid.rows) - 1 do
    for col = 0 to grid.cols - 1 do
      let current_cell = grid.cells.(Grid.cell_index grid ~row ~col) in
      let n = current_cell.len in

      if n >= 2 then
        for i = 0 to n - 2 do
          for j = i + 1 to n - 1 do
            let id1 = current_cell.data.(i) in
            let id2 = current_cell.data.(j) in
            let pair_key = pair_key_of_pairs id1 id2 in
            if not (Hashtbl.mem added_pairs pair_key) then (
              Hashtbl.add added_pairs pair_key ();
              Dynarray.add_last ids1 id1;
              Dynarray.add_last ids2 id2)
          done
        done;

      for dir = 0 to 3 do
        let new_col = col + dx.(dir) in
        let new_row = row + dy.(dir) in

        (* skip if neighbour is out of bounds *)
        if new_col >= 0 && new_col < grid.cols && new_row >= 0 && new_row < grid.rows then
          let adjacent_cell = grid.cells.(Grid.cell_index grid ~row:new_row ~col:new_col) in

          (* Pairs between current cell and neighbour *)
          for i = 0 to current_cell.len - 1 do
            let id1 = current_cell.data.(i) in

            for j = 0 to adjacent_cell.len - 1 do
              let id2 = adjacent_cell.data.(j) in
              if id1 <> id2 then
                let pair_key = pair_key_of_pairs id1 id2 in

                if not (Hashtbl.mem added_pairs pair_key) then (
                  Hashtbl.add added_pairs pair_key ();
                  Dynarray.add_last ids1 id1;
                  Dynarray.add_last ids2 id2)
            done
          done
      done
    done
  done

let update_broad_phase (s : Rb_store.t) (grid : Grid.t) =
  Grid.clear_grid grid;

  if s.len > 0 then
    for row = 0 to s.len - 1 do
      Grid.insert grid row ~min_x:s.min_x.(row) ~min_y:s.min_y.(row) ~max_x:s.max_x.(row)
        ~max_y:s.max_y.(row)
    done

let pairs_view c = (c.ids1, c.ids2)

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "broad_phase"
end)
