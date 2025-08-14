open Luma__storage
open Luma__id

type t = {
  id : Id.Archetype.t;
  mutable entities : Id.EntitySet.t;
  components : Id.ComponentSet.t;
  table : Component.packed Sparse_set.t Sparse_set.t;
}

let id a = a.id
let components a = a.components
let entities a = a.entities
let pp fmt a = Fmt.pf fmt "<Archetype id: %d>" @@ Id.Archetype.to_int a.id
let show a = Luma__core.Print.show_of_pp pp a

let create components =
  let id = Id.Archetype.next () in
  let table = Sparse_set.create () in
  Id.ComponentSet.iter
    (fun id -> Sparse_set.set table (Id.Component.to_int id) (Sparse_set.create ()))
    components;
  { components; id; entities = Id.EntitySet.empty; table }

let empty () = create Id.ComponentSet.empty

(* Search for the component sparse set and performs an action on it. Fails if the set cannot be found *)
let find_component_set_with_action arch component action =
  let c_id = Id.Component.to_int (Component.id component) in
  match Sparse_set.get arch.table c_id with
  | Some s -> action s
  | None ->
      raise
      @@ Luma__core.Error.component_not_found_exn c_id "Archetype.find_component_set_with_action"

let add arch entity components =
  let add () = arch.entities <- Id.EntitySet.add entity arch.entities in
  let entity_id = Id.Entity.to_int entity in
  components
  |> List.iter (fun component ->
         find_component_set_with_action arch component (fun set ->
             Sparse_set.set set entity_id component));
  add ()

let replace arch entity component =
  let entity_id = Id.Entity.to_int entity in
  find_component_set_with_action arch component (fun set -> Sparse_set.set set entity_id component)

let remove_entity arch entity =
  let remove () = arch.entities <- Id.EntitySet.remove entity arch.entities in
  let entity_id = Id.Entity.to_int entity in
  Id.ComponentSet.iter
    (fun c ->
      let c_id = Id.Component.to_int c in
      match Sparse_set.get arch.table c_id with
      | Some t -> Sparse_set.delete t entity_id
      | None -> ())
    arch.components;
  remove ()

let query_table arch entity component_id =
  let entity_id = Id.Entity.to_int entity in
  let component_id = Id.Component.to_int component_id in
  match Sparse_set.get arch.table component_id with
  | Some t -> ( match Sparse_set.get t entity_id with Some c -> Some c | None -> None)
  | None -> None
