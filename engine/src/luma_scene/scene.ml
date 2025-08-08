open Luma__id
open Luma__ecs
open Luma__type_register
open Luma__resource
open Types

let snapshot_world name world =
  let id = Id.Scene.next () in
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  let entities = World.Introspect.entities_seq world in

  let entities =
    Seq.fold_left
      (fun e_acc e_id ->
        let World.{ uuid = e_uuid; name = e_name } = World.entity_metadata world e_id in
        let components =
          Id.ComponentSet.fold
            (fun c_id c_acc ->
              match World.Introspect.get_component_packed world e_id c_id with
              | Some c -> c :: c_acc
              | None -> c_acc)
            (World.Introspect.entity_components world e_id)
            []
        in
        let (entity : entity) = { uuid = e_uuid; name = e_name; components } in
        entity :: e_acc)
      [] entities
  in

  let resources =
    World.Introspect.resources_seq world |> List.of_seq |> List.map (fun (_, packed) -> packed)
  in
  { id; uuid; name; entities; resources; version = 1 }

let inject_into_world scene world =
  scene.entities
  |> List.iter (fun (e : entity) ->
         let entity = World.add_entity ~name:e.name ~uuid:(Some e.uuid) world in
         e.components |> List.iter (fun c -> World.add_component world c entity);
         ());
  world

let inject_into_world_safe scene world =
  scene.entities
  |> List.iter (fun (e : entity) ->
         if World.has_entity_uuid world e.uuid then ()
         else
           let entity = World.add_entity ~name:e.name ~uuid:(Some e.uuid) world in
           e.components |> List.iter (fun c -> World.add_component world c entity);
           ());
  world

let to_world scene =
  let world = World.create () in
  inject_into_world scene world

module Serialize = Serialize
