open Luma__core
open Luma__id
open Luma__resource
module ArchetypeHashSet = Set.Make (Int)

let log = Luma__core.Log.sub_log "world"

type entity_metadata = {
  uuid : Uuidm.t;
  name : string;
}

type component_metadata = {
  id : Id.Component.t;
  name : string;
}

type t = {
  empty_archetype : Archetype.t;
  archetypes : (int, Archetype.t) Hashtbl.t;
  entity_to_archetype_lookup : (Id.Entity.t, int) Hashtbl.t;
  entity_id_to_metadata_lookup : (Id.Entity.t, entity_metadata) Hashtbl.t;
  entity_guid_to_entity_id_lookup : (Uuidm.t, Id.Entity.t) Hashtbl.t;
  component_to_archetype_lookup : (Id.Component.t, ArchetypeHashSet.t) Hashtbl.t;
  resources : (Id.Resource.t, Resource.packed) Hashtbl.t;
  mutable revision : int;
}

let create () =
  let archetypes = Hashtbl.create 16 in
  let empty_archetype = Archetype.empty () in
  Hashtbl.add archetypes (Archetype.hash empty_archetype) empty_archetype;
  {
    empty_archetype;
    archetypes;
    entity_to_archetype_lookup = Hashtbl.create 16;
    entity_id_to_metadata_lookup = Hashtbl.create 16;
    entity_guid_to_entity_id_lookup = Hashtbl.create 16;
    component_to_archetype_lookup = Hashtbl.create 16;
    resources = Hashtbl.create 16;
    revision = 0;
  }

let entities w = w.entity_to_archetype_lookup |> Hashtbl.to_seq_keys |> List.of_seq
let resources w = w.resources

let add_resource key res w =
  if Hashtbl.mem w.resources key then (
    let res_pp = Resource.show res in
    log.error (fun l -> l "Attempted to add resource %s more than once." res_pp);
    failwith @@ Printf.sprintf "Attempted to add resource %s more than once." res_pp)
  else Hashtbl.add w.resources key res;
  w

let set_resource key res w =
  Hashtbl.replace w.resources key res;
  w

let has_resource key w = Hashtbl.mem w.resources key
let get_resource w key = Hashtbl.find_opt w.resources key
let archetypes w = w.archetypes
let entity_metadata w e = Hashtbl.find w.entity_id_to_metadata_lookup e
let has_entity_uuid w uuid = Hashtbl.mem w.entity_guid_to_entity_id_lookup uuid

let add_entity ?(name = "") ?(uuid = None) w =
  let entity = Entity.make ~uuid name in
  if Option.is_some uuid && Hashtbl.mem w.entity_guid_to_entity_id_lookup (Option.get uuid) then (
    let message =
      Printf.sprintf "The uuid %s already exists in the world." (Uuidm.to_string @@ Option.get uuid)
    in
    log.error (fun l -> l "%s" message);
    failwith message)
  else
    (* these calls "should" never raise *)
    let e_id = Entity.id entity in
    let metadata = { uuid = Entity.uuid entity; name } in
    Archetype.add w.empty_archetype e_id [];
    Hashtbl.replace w.entity_to_archetype_lookup e_id (Archetype.hash w.empty_archetype);
    Hashtbl.replace w.entity_id_to_metadata_lookup e_id metadata;
    Hashtbl.replace w.entity_guid_to_entity_id_lookup (Entity.uuid entity) e_id;
    w.revision <- w.revision + 1;
    e_id

let get_new_archetype w old_archetype operation =
  let hash = Archetype.next_hash old_archetype operation in
  match Hashtbl.find_opt w.archetypes hash with
  | Some a -> a
  | None ->
      let component_id, operation =
        match operation with
        | Add id -> (id, Id.ComponentSet.add)
        | Remove id -> (id, Id.ComponentSet.remove)
      in
      let new_archetype =
        Archetype.create (operation component_id (Archetype.components old_archetype))
      in
      Hashtbl.add w.archetypes hash new_archetype;
      new_archetype

let update_component_to_arch w archetype =
  let operation =
    if Id.EntitySet.is_empty (Archetype.entities archetype) then ArchetypeHashSet.remove
    else ArchetypeHashSet.add
  in
  Archetype.components archetype
  |> Id.ComponentSet.iter (fun cid ->
         let arch_set =
           Option.value
             (Hashtbl.find_opt w.component_to_archetype_lookup cid)
             ~default:ArchetypeHashSet.empty
         in
         Hashtbl.replace w.component_to_archetype_lookup cid
           (operation (Archetype.hash archetype) arch_set))

let find_archetype w entity =
  match Hashtbl.find_opt w.entity_to_archetype_lookup entity with
  | Some hash -> Hashtbl.find w.archetypes hash
  | None ->
      raise
      @@ Error.entity_not_found_exn (Id.Entity.to_int entity) "find_archetype in add_component"

let add_component w component entity =
  let old_arch = find_archetype w entity in
  let new_archetype = get_new_archetype w old_arch (Archetype.Add (Component.id component)) in
  if Archetype.hash old_arch = Archetype.hash new_archetype then
    Archetype.replace old_arch entity component
  else (
    Hashtbl.replace w.entity_to_archetype_lookup entity (Archetype.hash new_archetype);
    (* TODO: validation *)
    let new_components =
      [ component ]
      @ List.filter_map
          (fun component_id -> Archetype.query_table old_arch entity component_id)
          (Id.ComponentSet.to_list (Archetype.components new_archetype))
    in
    Archetype.remove_entity old_arch entity;
    Archetype.add new_archetype entity new_components;
    update_component_to_arch w old_arch;
    update_component_to_arch w new_archetype;
    w.revision <- w.revision + 1)

let with_component (type a) w (module C : Component.S with type t = a) component entity =
  let packed = Component.pack (module C) component in
  add_component w packed entity;
  entity

let query w ?(filter = Query.Component.Filter.Any) query =
  let archetypes = w.archetypes |> Hashtbl.to_seq_values |> List.of_seq in
  (* TODO: fail or ...? *)
  Query.Component.evaluate ~filter query archetypes |> Result.value ~default:[]

let get_component (type a) w (module C : Component.S with type t = a) e =
  let arch = find_archetype w e in
  match Archetype.query_table arch e C.id with
  | Some p -> Component.unpack_opt (module C) p
  | None -> None

module Introspect = struct
  let revision w = w.revision
  let iter_entities f w = Hashtbl.iter (fun e _ -> f e) w.entity_to_archetype_lookup
  let entities_seq w = Hashtbl.to_seq_keys w.entity_to_archetype_lookup
  let entity_components w e = Archetype.components (find_archetype w e)

  let get_component_packed w e cid =
    let arch = find_archetype w e in
    Archetype.query_table arch e cid

  let iter_entities_with_component f w cid =
    match Hashtbl.find_opt w.component_to_archetype_lookup cid with
    | None -> ()
    | Some arches ->
        ArchetypeHashSet.iter
          (fun h ->
            match Hashtbl.find_opt w.archetypes h with
            | None -> ()
            | Some a -> Id.EntitySet.iter f (Archetype.entities a))
          arches

  let resources_seq w = Hashtbl.to_seq w.resources
end
