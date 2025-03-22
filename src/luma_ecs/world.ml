module ArchetypeHashSet = Set.Make (Int)

type t = {
  empty_archetype : Archetype.t;
  archetypes : (int, Archetype.t) Hashtbl.t;
  entity_to_archetype_lookup : (Luma__id.Id.Entity.t, int) Hashtbl.t;
  component_to_archetype_lookup : (Luma__id.Id.Component.t, ArchetypeHashSet.t) Hashtbl.t;
  resources : (Luma__id.Id.Resource.t, Luma__resource.Resource.packed) Hashtbl.t;
}

let create () =
  let archetypes = Hashtbl.create 16 in
  let empty_archetype = Archetype.empty () in
  Hashtbl.add archetypes (Archetype.hash empty_archetype) empty_archetype;
  {
    empty_archetype;
    archetypes;
    entity_to_archetype_lookup = Hashtbl.create 16;
    component_to_archetype_lookup = Hashtbl.create 16;
    resources = Hashtbl.create 16;
  }

let resources w = w.resources

let add_resource key res w =
  Hashtbl.add w.resources key res;
  w

let get_resource w key = Hashtbl.find_opt w.resources key
let archetypes w = w.archetypes

let add_entity w =
  let entity = Luma__id.Id.Entity.next () in
  Archetype.add w.empty_archetype entity [];
  Hashtbl.replace w.entity_to_archetype_lookup entity (Archetype.hash w.empty_archetype);
  entity

let get_archetype w entity =
  (match Hashtbl.find_opt w.entity_to_archetype_lookup entity with
  | Some hash -> Hashtbl.find_opt w.archetypes hash
  | None -> None)
  |> Option.to_result ~none:"entity or archetype could not be found"

let get_new_archetype w old_archetype operation =
  let hash = Archetype.next_hash old_archetype operation in
  match Hashtbl.find_opt w.archetypes hash with
  | Some a -> a
  | None ->
      let component_id, operation =
        match operation with
        | Add id -> (id, Luma__id.Id.ComponentSet.add)
        | Remove id -> (id, Luma__id.Id.ComponentSet.remove)
      in
      let new_archetype =
        Archetype.create (operation component_id (Archetype.components old_archetype))
      in
      Hashtbl.add w.archetypes hash new_archetype;
      new_archetype

let update_component_to_arch w archetype =
  let operation =
    if Luma__id.Id.EntitySet.is_empty (Archetype.entities archetype) then
      ArchetypeHashSet.remove
    else
      ArchetypeHashSet.add
  in
  Archetype.components archetype
  |> Luma__id.Id.ComponentSet.iter (fun cid ->
         let arch_set =
           Option.value
             (Hashtbl.find_opt w.component_to_archetype_lookup cid)
             ~default:ArchetypeHashSet.empty
         in
         Hashtbl.replace w.component_to_archetype_lookup cid
           (operation (Archetype.hash archetype) arch_set))

let add_component w component entity =
  match get_archetype w entity with
  | Ok old_archetype ->
      let new_archetype =
        get_new_archetype w old_archetype (Archetype.Add (Component.id component))
      in
      if Archetype.hash old_archetype = Archetype.hash new_archetype then
        Archetype.replace old_archetype entity component
      else (
        Hashtbl.replace w.entity_to_archetype_lookup entity (Archetype.hash new_archetype);
        (* TODO: validation *)
        let new_components =
          [ component ]
          @ List.filter_map
              (fun component_id -> Archetype.query_table old_archetype entity component_id)
              (Luma__id.Id.ComponentSet.to_list (Archetype.components new_archetype))
        in
        Archetype.remove_entity old_archetype entity;
        Archetype.add new_archetype entity new_components;
        update_component_to_arch w old_archetype;
        update_component_to_arch w new_archetype)
  | Error e -> failwith e

let with_component : type a.
    t -> (module Component.S with type t = a) -> a -> Luma__id.Id.Entity.t -> Luma__id.Id.Entity.t =
 fun w (module C) component entity ->
  let packed = Component.pack (module C) component in
  add_component w packed entity;
  entity

let query : t -> ?filter:Query.Filter.t -> 'a Query.t -> (Luma__id.Id.Entity.t * 'a) list =
 fun w ?(filter = Query.Filter.Any) query ->
  let archetypes = w.archetypes |> Hashtbl.to_seq_values |> List.of_seq in
  Query.evaluate ~filter query archetypes
