module Id = Luma__id.Id
open Luma__storage

type t = {
  mutable entities : Id.EntitySet.t;
  components : Id.ComponentSet.t;
  hash : int;
  table : Component.packed Sparse_set.t Sparse_set.t;
  add_hashes_cache : (Id.Component.t, int) Hashtbl.t;
  remove_hashes_cache : (Id.Component.t, int) Hashtbl.t;
}

type operation = Add of Id.Component.t | Remove of Id.Component.t

(* Convert to a string before hashing to get around the depth limit of 10 on Hashtbl.hash. *)
let generate_hash (components : Id.Component.t list) =
  components
  |> List.sort compare
  |> List.map (fun v -> v |> Id.Component.to_int |> string_of_int)
  |> String.concat ""
  |> Hashtbl.hash

let create components =
  let hash = generate_hash (Id.ComponentSet.to_list components) in
  let table = Sparse_set.create () in
  let add_hashes_cache = Hashtbl.create 0 in
  let remove_hashes_cache = Hashtbl.create 0 in
  Id.ComponentSet.iter
    (fun id -> Sparse_set.set table (Id.Component.to_int id) (Sparse_set.create ()))
    components;
  { components; hash; entities = Id.EntitySet.empty; table; add_hashes_cache; remove_hashes_cache }

let empty () = create Id.ComponentSet.empty

(* TODO: Validation  *)
let add a entity_id components =
  let add () = a.entities <- Id.EntitySet.add entity_id a.entities in
  let entity_id = Id.Entity.to_int entity_id in
  components
  |> List.iter (fun c ->
         let c_id = Id.Component.to_int (Component.id c) in
         match Sparse_set.get a.table c_id with
         | Some s -> Sparse_set.set s entity_id c
         | None -> failwith "no component");
  add ()

let replace a entity_id component =
  let entity_id = Id.Entity.to_int entity_id in
  let component_id = Id.Component.to_int (Component.id component) in
  match Sparse_set.get a.table component_id with
  | Some comp_set -> Sparse_set.set comp_set entity_id component
  | None -> invalid_arg "could not find component"

let remove_entity a entity_id =
  let remove () = a.entities <- Id.EntitySet.remove entity_id a.entities in
  let entity_id = Id.Entity.to_int entity_id in
  Id.ComponentSet.iter
    (fun c ->
      let c_id = Id.Component.to_int c in
      match Sparse_set.get a.table c_id with Some t -> Sparse_set.delete t entity_id | None -> ())
    a.components;
  remove ()

let next_hash a operation =
  let find_or_create tbl key generate_func =
    match Hashtbl.find_opt tbl key with
    | Some h -> h
    | None ->
        let h = generate_func () in
        Hashtbl.add tbl key h;
        h
  in
  match operation with
  | Add component_id ->
      if Id.ComponentSet.mem component_id a.components then
        a.hash
      else
        find_or_create a.add_hashes_cache component_id (fun () ->
            generate_hash
            @@ Id.ComponentSet.to_list
            @@ Id.ComponentSet.add component_id a.components)
  | Remove component_id ->
      if not @@ Id.ComponentSet.mem component_id a.components then
        a.hash
      else
        find_or_create a.remove_hashes_cache component_id (fun () ->
            generate_hash
            @@ Id.ComponentSet.to_list
            @@ Id.ComponentSet.remove component_id a.components)

let query_table a entity_id component_id =
  let entity_id = Id.Entity.to_int entity_id in
  let component_id = Id.Component.to_int component_id in
  match Sparse_set.get a.table component_id with
  | Some t -> ( match Sparse_set.get t entity_id with Some c -> Some c | None -> None)
  | None -> None

let hash a = a.hash
let components a = a.components
let entities a = a.entities
