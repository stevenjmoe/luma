open Luma__core
open Luma__id
open Luma__resource

module CompSetTbl = Hashtbl.Make (struct
  type t = Id.ComponentSet.t

  let equal = Id.ComponentSet.equal

  let hash (s : t) =
    let open Int64 in
    let seed = 0x9e3779b97f4a7c15L in
    Id.ComponentSet.fold
      (fun cid acc ->
        let h = of_int @@ Id.Component.to_int cid in
        let acc_shift = add (shift_left acc 6) (shift_right_logical acc 2) in
        logxor acc (add h (add seed acc_shift)))
      s 0L
    |> to_int
end)

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
  archetypes : (Id.Archetype.t, Archetype.t) Hashtbl.t;
  entity_to_archetype : (Id.Entity.t, Id.Archetype.t) Hashtbl.t;
  entity_id_to_metadata : (Id.Entity.t, entity_metadata) Hashtbl.t;
  entity_guid_to_entity_id : (Uuidm.t, Id.Entity.t) Hashtbl.t;
  component_to_archetype : (Id.Component.t, ArchetypeHashSet.t) Hashtbl.t;
  resources : (Id.Resource.t, Resource.packed) Hashtbl.t;
  archetype_by_sig : Archetype.t CompSetTbl.t;
  mutable revision : int;
}

let incr_revision world = world.revision <- world.revision + 1

let create () =
  let archetypes = Hashtbl.create 16 in
  let empty_archetype = Archetype.empty () in
  let archetype_by_sig = CompSetTbl.create 16 in
  CompSetTbl.add archetype_by_sig (Archetype.components empty_archetype) empty_archetype;

  Hashtbl.add archetypes (Archetype.id empty_archetype) empty_archetype;
  {
    empty_archetype;
    archetypes;
    entity_to_archetype = Hashtbl.create 16;
    entity_id_to_metadata = Hashtbl.create 16;
    entity_guid_to_entity_id = Hashtbl.create 16;
    component_to_archetype = Hashtbl.create 16;
    resources = Hashtbl.create 16;
    archetype_by_sig;
    revision = 0;
  }

let entities w = w.entity_to_archetype |> Hashtbl.to_seq_keys |> List.of_seq
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
let entity_metadata w e = Hashtbl.find w.entity_id_to_metadata e
let has_entity_uuid w uuid = Hashtbl.mem w.entity_guid_to_entity_id uuid

let get_or_create_archetype_by_sig world components =
  match CompSetTbl.find_opt world.archetype_by_sig components with
  | Some a -> a
  | None ->
      let a = Archetype.create components in
      Hashtbl.add world.archetypes (Archetype.id a) a;
      CompSetTbl.add world.archetype_by_sig components a;
      a

let entity_uuid_exists world uuid =
  match uuid with Some uuid -> Hashtbl.mem world.entity_guid_to_entity_id uuid | None -> false

let add_entity ?(name = "") ?(uuid = None) world =
  let entity = Entity.make ~uuid name in

  if entity_uuid_exists world uuid then (
    let message =
      Printf.sprintf "The uuid %s already exists in the world." (Uuidm.to_string @@ Option.get uuid)
    in
    log.error (fun l -> l "%s" message);
    failwith message)
  else
    (* these calls "should" never raise *)
    let e_id = Entity.id entity in
    let metadata = { uuid = Entity.uuid entity; name } in
    Archetype.add world.empty_archetype e_id [];
    Hashtbl.replace world.entity_to_archetype e_id (Archetype.id world.empty_archetype);
    Hashtbl.replace world.entity_id_to_metadata e_id metadata;
    Hashtbl.replace world.entity_guid_to_entity_id (Entity.uuid entity) e_id;

    incr_revision world;
    e_id

let update_component_to_arch world archetype =
  let operation =
    if Id.EntitySet.is_empty (Archetype.entities archetype) then ArchetypeHashSet.remove
    else ArchetypeHashSet.add
  in
  Id.ComponentSet.iter (fun cid ->
      let arch_set =
        Option.value
          (Hashtbl.find_opt world.component_to_archetype cid)
          ~default:ArchetypeHashSet.empty
      in
      Hashtbl.replace world.component_to_archetype cid
        (operation (Archetype.id archetype |> Id.Archetype.to_int) arch_set))
  @@ Archetype.components archetype

(* TODO: find_archetype_exn and a version that returns an option *)
let find_archetype world entity =
  match Hashtbl.find_opt world.entity_to_archetype entity with
  | Some id -> Hashtbl.find world.archetypes id
  | None -> raise @@ Error.entity_not_found_exn (Id.Entity.to_int entity) "World.find_archetype"

let query world ?(filter = Query.Component.Filter.Any) query =
  let archetypes = world.archetypes |> Hashtbl.to_seq_values |> List.of_seq in
  Query.Component.evaluate ~filter query archetypes |> Result.value ~default:[]

let get_component (type a) world (module C : Component.S with type t = a) entity =
  let arch = find_archetype world entity in
  match Archetype.query_table arch entity C.id with
  | Some p -> Component.unpack_opt (module C) p
  | None -> None

let has_component (type a) world (module C : Component.S with type t = a) entity =
  let arch = find_archetype world entity in
  match Archetype.query_table arch entity C.id with Some p -> true | None -> false

let move_entity_to_archetype world entity ~old_arch ~new_arch overrides =
  if Archetype.id old_arch = Archetype.id new_arch then (
    Hashtbl.iter (fun _ packed -> Archetype.replace old_arch entity packed) overrides;
    incr_revision world)
  else
    let wanted_ids = Id.ComponentSet.to_list (Archetype.components new_arch) in
    let payloads =
      List.filter_map
        (fun cid ->
          match Hashtbl.find_opt overrides cid with
          | Some p -> Some p
          | None -> Archetype.query_table old_arch entity cid)
        wanted_ids
    in

    Archetype.remove_entity old_arch entity;
    Archetype.add new_arch entity payloads;
    Hashtbl.replace world.entity_to_archetype entity (Archetype.id new_arch);
    update_component_to_arch world old_arch;
    update_component_to_arch world new_arch;
    incr_revision world

(*TODO: check dupes*)
let add_component world component entity =
  let old_arch = find_archetype world entity in
  let old_sig = Archetype.components old_arch in
  let cid = Component.id component in

  if Id.ComponentSet.mem cid old_sig then (
    Archetype.replace old_arch entity component;
    incr_revision world)
  else
    let new_sig = Id.ComponentSet.add cid old_sig in
    let new_arch = get_or_create_archetype_by_sig world new_sig in
    let overrides = Hashtbl.create 1 in
    Hashtbl.replace overrides cid component;
    move_entity_to_archetype world entity ~old_arch ~new_arch overrides

let with_component (type a) w (module C : Component.S with type t = a) component entity =
  let packed = Component.pack (module C) component in
  add_component w packed entity;
  entity

let add_components world entity components =
  if components = [] then ()
  else
    let old_arch = find_archetype world entity in
    let overrides = Hashtbl.create (List.length components) in
    let target_sig =
      List.fold_left
        (fun s packed -> Id.ComponentSet.add (Component.id packed) s)
        (Archetype.components old_arch) components
    in
    let new_arch = get_or_create_archetype_by_sig world target_sig in
    List.iter (fun p -> Hashtbl.replace overrides (Component.id p) p) components;
    move_entity_to_archetype world entity ~old_arch ~new_arch overrides

let add_entity_with_components world ?(name = "") ?(uuid = None) components =
  if entity_uuid_exists world uuid then (
    let message =
      Printf.sprintf "The uuid %s already exists in the world." (Uuidm.to_string @@ Option.get uuid)
    in
    log.error (fun l -> l "%s" message);
    failwith message)
  else
    let entity = Entity.make ~uuid name in
    let meta = { uuid = Entity.uuid entity; name } in

    let sigset =
      List.fold_left
        (fun s p -> Id.ComponentSet.add (Component.id p) s)
        Id.ComponentSet.empty components
    in
    let e_id = Entity.id entity in
    let arch = get_or_create_archetype_by_sig world sigset in

    Archetype.add arch e_id components;
    Hashtbl.replace world.entity_to_archetype e_id (Archetype.id arch);
    Hashtbl.replace world.entity_id_to_metadata e_id meta;
    Hashtbl.replace world.entity_guid_to_entity_id (Entity.uuid entity) e_id;
    update_component_to_arch world arch;
    incr_revision world;
    e_id

module Introspect = struct
  let revision w = w.revision
  let iter_entities f w = Hashtbl.iter (fun e _ -> f e) w.entity_to_archetype
  let entities_seq w = Hashtbl.to_seq_keys w.entity_to_archetype
  let entity_components w e = Archetype.components (find_archetype w e)

  let get_component_packed w e cid =
    let arch = find_archetype w e in
    Archetype.query_table arch e cid

  let iter_entities_with_component f w cid =
    match Hashtbl.find_opt w.component_to_archetype cid with
    | None -> ()
    | Some arches ->
        ArchetypeHashSet.iter
          (fun h ->
            match Hashtbl.find_opt w.archetypes (Id.Archetype.of_int h) with
            | None -> ()
            | Some a -> Id.EntitySet.iter f (Archetype.entities a))
          arches

  let resources_seq w = Hashtbl.to_seq w.resources
end
