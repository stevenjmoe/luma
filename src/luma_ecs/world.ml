module ArchetypeHashSet = Set.Make (Int)

let log = Luma__core.Log.sub_log "world"

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
  if Hashtbl.mem w.resources key then (
    let res_pp = Luma__resource.Resource.show res in
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

let add_entity w =
  let entity = Luma__id.Id.Entity.next () in
  (* these calls "should" never raise *)
  Archetype.add w.empty_archetype entity [];
  Hashtbl.replace w.entity_to_archetype_lookup entity (Archetype.hash w.empty_archetype);
  entity

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
    if Luma__id.Id.EntitySet.is_empty (Archetype.entities archetype) then ArchetypeHashSet.remove
    else ArchetypeHashSet.add
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

let find_archetype w entity =
  match Hashtbl.find_opt w.entity_to_archetype_lookup entity with
  | Some hash -> Hashtbl.find w.archetypes hash
  | None ->
      raise
      @@ Luma__core.Error.entity_not_found_exn
           (Luma__id.Id.Entity.to_int entity)
           "find_archetype in add_component"

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
          (Luma__id.Id.ComponentSet.to_list (Archetype.components new_archetype))
    in
    Archetype.remove_entity old_arch entity;
    Archetype.add new_archetype entity new_components;
    update_component_to_arch w old_arch;
    update_component_to_arch w new_archetype)

let with_component : type a.
    t -> (module Component.S with type t = a) -> a -> Luma__id.Id.Entity.t -> Luma__id.Id.Entity.t =
 fun w (module C) component entity ->
  let packed = Component.pack (module C) component in
  add_component w packed entity;
  entity

let query :
    t ->
    ?filter:Query.Component.Filter.t ->
    'a Query.Component.t ->
    (Luma__id.Id.Entity.t * 'a) list =
 fun w ?(filter = Query.Component.Filter.Any) query ->
  let archetypes = w.archetypes |> Hashtbl.to_seq_values |> List.of_seq in
  (* TODO: fail or ...? *)
  Query.Component.evaluate ~filter query archetypes |> Result.value ~default:[]

let get_component (type a) w (module C : Component.S with type t = a) e =
  let arch = find_archetype w e in
  match Archetype.query_table arch e C.id with
  | Some p -> Component.unpack_opt (module C) p
  | None -> None
