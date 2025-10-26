open Containers

type 'a t = {
  sparse : int option Vector.vector Vector.vector;
  dense : 'a Vector.vector;
  max_size : int;
  dense_to_entity : int Vector.vector;
}

let vector_get_opt v idx =
  if idx >= 0 && idx < Vector.length v then Some (Vector.get v idx) else None

let create ?max_size () =
  let max_size = match max_size with Some s -> s | None -> 2048 in
  {
    sparse = Vector.create ();
    dense = Vector.create ();
    max_size;
    dense_to_entity = Vector.create ();
  }
  |> fun t ->
  Vector.push t.sparse (Vector.create ());
  t

let ensure_page_exists t page =
  if page >= Vector.length t.sparse then
    Vector.resize_with_init t.sparse ~init:(Vector.create ()) (page + 1)

let ensure_index_exists page_vec sparse_index =
  if sparse_index >= Vector.length page_vec then
    Vector.resize_with_init page_vec ~init:None (sparse_index + 1)

let get_page_and_index entity_id max_size =
  let page = entity_id / max_size in
  let sparse_index = entity_id mod max_size in
  (page, sparse_index)

let set_dense_index t entity_id idx () =
  let page, sparse_index = get_page_and_index entity_id t.max_size in
  ensure_page_exists t page |> ignore;
  let page_vec = Vector.get t.sparse page in
  ensure_index_exists page_vec sparse_index |> ignore;
  (* Resize the inner value array *)
  Vector.set page_vec sparse_index idx

let get_dense_index t entity_id =
  let page, sparse_index = get_page_and_index entity_id t.max_size in
  if page < Vector.length t.sparse then
    let sparse_page = Vector.get t.sparse page in
    match vector_get_opt sparse_page sparse_index with Some r -> r | None -> None
  else None

let set t entity_id value =
  match get_dense_index t entity_id with
  | Some idx ->
      Vector.set t.dense idx value;
      Vector.set t.dense_to_entity idx entity_id
  | _ ->
      set_dense_index t entity_id (Some (Vector.length t.dense)) ();
      Vector.push t.dense value;
      Vector.push t.dense_to_entity entity_id

let get t entity_id =
  match get_dense_index t entity_id with Some idx -> vector_get_opt t.dense idx | _ -> None

let delete t entity_id =
  match get_dense_index t entity_id with
  | Some idx ->
      let last_idx = Vector.length t.dense - 1 in

      (* If we're deleting the last element, just pop it *)
      if idx = last_idx then (
        set_dense_index t entity_id None ();
        Vector.pop t.dense |> ignore;
        Vector.pop t.dense_to_entity |> ignore)
      else (* Swap the last element with the one being removed *)
        let last_value = Vector.get t.dense last_idx in
        let last_entity = Vector.get t.dense_to_entity last_idx in

        (* Update dense arrays *)
        Vector.set t.dense idx last_value;
        Vector.set t.dense_to_entity idx last_entity;

        (* Update sparse array for the swapped entity *)
        set_dense_index t last_entity (Some idx) ();

        (* Remove the last element *)
        set_dense_index t entity_id None ();
        Vector.pop t.dense |> ignore;
        Vector.pop t.dense_to_entity |> ignore
  | _ -> ()

let dense s = s.dense
let sparse s = s.sparse
let max_size s = s.max_size
let length s = Vector.length s.dense

let%test "resize" =
  let s_set = create () in
  set s_set 100000 69;
  let x = Option.value (get s_set 100000) ~default:(-1) in
  x = 69

let%test "add things to inner" =
  let s_set = create () in
  let s_set2 = create () in
  set s_set2 3 69;
  set s_set 9 s_set2;
  let s = get s_set 9 |> Option.value ~default:(create ()) in
  let x = Option.value (get s 3) ~default:(-1) in
  x = 69
