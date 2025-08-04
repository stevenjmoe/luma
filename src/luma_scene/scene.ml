open Luma__id
open Luma__ecs
open Luma__type_register
open Luma__resource
open Luma__serialize
open Luma__type_register.Type_register

type entity = {
  uuid : Uuidm.t;
  name : string;
  components : Component.packed list;
}

type t = {
  id : Id.Scene.t;
  uuid : Uuidm.t;
  name : string;
  entities : entity list;
}

let from_world name world =
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
  { id; uuid; name; entities }

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

let serialize_json (type a b) scene world : (Yojson.Safe.t, string) result =
  match World.get_resource world Component_registry.R.type_id with
  | None -> Error "todo"
  | Some r -> (
      match Resource.unpack (module Component_registry.R) r with
      | Error _ -> Error "todo"
      | Ok r ->
          let entities =
            List.map
              (fun (e : entity) ->
                let components =
                  e.components
                  |> List.filter_map (fun c ->
                         match Component_registry.get_entry r (Component.name c) with
                         | Some (Component { name; serializers; instance = (module C) }) ->
                             let unpacked = Component.unpack (module C) c |> Result.get_ok in
                             let (module Q) =
                               Type_register.Component_registry.get_json_serializer serializers
                               |> Option.get
                             in
                             Some (Q.serialize unpacked)
                         | None -> None)
                in
                `Assoc
                  [
                    ("uuid", `String (Uuidm.to_string e.uuid));
                    ("name", `String e.name);
                    ("data", `List components);
                  ])
              scene.entities
          in
          let r =
            `Assoc
              [
                ("name", `String scene.name);
                ("uuid", `String (Uuidm.to_string scene.uuid));
                ("entities", `List entities);
              ]
          in
          Yojson.Safe.pretty_to_channel Out_channel.stdout r;
          Ok r)
