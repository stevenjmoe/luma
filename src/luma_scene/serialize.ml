open Luma__id
open Luma__ecs
open Luma__type_register
open Luma__resource
open Luma__serialize
open Luma__type_register.Type_register
open Types
open Yojson.Safe.Util

module Json = struct
  let serialize_entities scene world =
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
                                 Type_register.get_json_serializer serializers |> Option.get
                               in
                               Some (Q.serialize unpacked)
                           | None -> None)
                  in
                  `Assoc
                    [
                      ("uuid", `String (Uuidm.to_string e.uuid));
                      ("name", `String e.name);
                      ("components", `List components);
                    ])
                scene.entities
            in
            let r = `List entities in
            Ok r)

  let serialize_resources scene world =
    match World.get_resource world Resource_registry.R.type_id with
    | None -> Error "TODO2"
    | Some packed_registry -> (
        match Resource.unpack (module Resource_registry.R) packed_registry with
        | Error _ -> Error "todo"
        | Ok registry ->
            let resources =
              List.filter_map
                (fun packed ->
                  let name = Resource.name packed in
                  match Resource_registry.get_entry registry name with
                  | Some (Resource { name; serializers; instance = (module R) }) ->
                      let unpacked = Resource.unpack (module R) packed |> Result.get_ok in
                      let (module Q) =
                        Type_register.get_json_serializer serializers |> Option.get
                      in
                      Some (Q.serialize unpacked)
                  | None -> None)
                scene.resources
            in

            Ok resources)

  let serialize (type a) scene world : (Yojson.Safe.t, string) result =
    let entities = serialize_entities scene world in
    let resources = serialize_resources scene world in
    match (entities, resources) with
    | Ok entities, Ok resources ->
        let r =
          `Assoc
            [
              ("name", `String scene.name);
              ("uuid", `String (Uuidm.to_string scene.uuid));
              ("entities", entities);
              ("resources", `List resources);
            ]
        in
        Ok r
    | _ -> Error "todo"

  let parse_string_field key json =
    match member key json with
    | `String v -> Ok v
    | _ -> Error (Printf.sprintf "Expected string field '%s'" key)

  let parse_uuid_field key json =
    match member key json with
    | `String v -> (
        match Uuidm.of_string v with
        | Some u -> Ok u
        | None -> Error (Printf.sprintf "Invalid uuid string in field '%s'" key))
    | _ -> Error (Printf.sprintf "Expected uuid string field '%s'" key)

  let parse_list_field key json =
    match member key json with
    | `List l -> Ok l
    | _ -> Error (Printf.sprintf "Expected list field '%s'" key)

  let extract_single_assoc obj =
    match obj with
    | `Assoc [ (name, data) ] -> Ok (name, data)
    | _ -> Error "Each assoc entry must be a single-field object."

  let parse_assoc_field key json =
    match Yojson.Safe.Util.member key json with
    | `Assoc assoc -> Ok assoc
    | _ -> Error (Printf.sprintf "Expected member '%s' to be a JSON object with named fields" key)

  (* Converts a list of results into a result of a list, returning the first error encountered or all successful values. *)
  let rec result_list_seq = function
    | [] -> Ok []
    | Ok x :: xs -> result_list_seq xs |> Result.map (fun rest -> x :: rest)
    | Error e :: _ -> Error e

  let deserialize (scene : Yojson.Safe.t) world =
    let open Yojson.Safe in
    let ( let* ) = Result.bind in
    let* r =
      World.get_resource world Component_registry.R.type_id
      |> Option.to_result ~none:"Could not find Component registry in World resources."
    in

    (* create a new world and copy across the Component registry *)
    let world = World.create () |> World.add_resource Component_registry.R.type_id r in
    let* name = parse_string_field "name" scene in
    let* uuid = parse_uuid_field "uuid" scene in
    let* entities_json = parse_list_field "entities" scene in
    let* resources_json = parse_list_field "resources" scene in

    let* entities =
      result_list_seq
      @@ List.map
           (fun e ->
             let* entity_name = parse_string_field "name" e in
             let* entity_uuid = parse_uuid_field "uuid" e in
             let* components = parse_list_field "components" e in

             let entity = World.add_entity ~name:entity_name ~uuid:(Some entity_uuid) world in

             let* components =
               result_list_seq
               @@ List.map
                    (fun component_json ->
                      let* component_name, component_data = extract_single_assoc component_json in

                      let* registry_packed =
                        World.get_resource world Component_registry.R.type_id
                        |> Option.to_result
                             ~none:"Could not find Component registry in World resources."
                      in

                      let* registry =
                        Resource.unpack (module Component_registry.R) registry_packed
                        |> Result.map_error (fun e ->
                               "Failed to unpack component registry while deserializing scene.")
                      in

                      let* (Component { instance = (module C); serializers }) =
                        Component_registry.get_entry registry component_name
                        |> Option.to_result
                             ~none:
                               (Printf.sprintf
                                  "Component '%s' has not been registered for \
                                   serialization/deserialization."
                                  component_name)
                      in

                      let* (module S) =
                        Type_register.get_json_serializer serializers
                        |> Option.to_result
                             ~none:
                               (Printf.sprintf
                                  "Component '%s' does not have a registered serializer."
                                  entity_name)
                      in

                      let* repr = S.deserialize component_json in
                      let packed = Component.pack (module C) repr in
                      World.add_component world packed entity;
                      Ok packed)
                    components
             in
             Ok { uuid; name = entity_name; components })
           entities_json
    in
    let id = Id.Scene.next () in
    Ok { id; uuid; name; entities; resources = [] }
end
