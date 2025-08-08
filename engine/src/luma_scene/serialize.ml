open Luma__id
open Luma__ecs
open Luma__type_register
open Luma__resource
open Luma__serialize
open Luma__type_register.Type_register
open Luma__core
open Types
open Yojson.Safe.Util

module Json = struct
  let log = Log.sub_log "json_serialize"

  let serialize_entities scene world =
    let ( let* ) = Result.bind in
    let* reg_packed =
      World.get_resource world Component_registry.R.type_id
      |> Option.to_result ~none:(Error.resource_not_found "Component registry not found.")
    in
    let* reg = Resource.unpack (module Component_registry.R) reg_packed in
    let entities =
      List.map
        (fun (e : entity) ->
          let components =
            e.components
            (* For the time being this just skips components that haven't been registered. *)
            |> List.filter_map (fun c ->
                   let ( let** ) = Option.bind in

                   let** (Component { name; serializers; instance = (module C) }) =
                     Component_registry.get_entry_by_name reg (Component.name c)
                   in
                   let** unpacked = Component.unpack_opt (module C) c in
                   let** (module Q) = Type_register.get_json_serializer serializers in

                   Some (Q.serialize unpacked))
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
    Ok r

  let serialize_resources scene world =
    let ( let* ) = Result.bind in
    let* packed_registry =
      World.get_resource world Resource_registry.R.type_id
      |> Option.to_result ~none:(Error.resource_not_found "Resource registry not found.")
    in
    let* registry = Resource.unpack (module Resource_registry.R) packed_registry in

    let resources =
      (* For the time being this just skips resources that haven't been registered. *)
      List.filter_map
        (fun packed ->
          let ( let** ) = Option.bind in

          let name = Resource.name packed in
          let** (Resource { name; serializers; instance = (module R) }) =
            Resource_registry.get_entry registry name
          in

          let** unpacked = Resource.unpack (module R) packed |> Result.to_option in
          let** (module Q) = Type_register.get_json_serializer serializers in

          Some (Q.serialize unpacked))
        scene.resources
    in

    Ok resources

  let serialize (type a) scene world =
    let entities = serialize_entities scene world in
    let resources = serialize_resources scene world in
    match (entities, resources) with
    | Ok entities, Ok resources ->
        let r =
          `Assoc
            [
              ("name", `String scene.name);
              ("uuid", `String (Uuidm.to_string scene.uuid));
              ("version", `Int scene.version);
              ("entities", entities);
              ("resources", `List resources);
            ]
        in
        Ok r
    | Error e, _ -> Error e
    | _, Error e -> Error e

  (* Converts a list of results into a result of a list, returning the first error encountered or all successful values. *)
  let rec result_list_seq = function
    | [] -> Ok []
    | Ok x :: xs -> result_list_seq xs |> Result.map (fun rest -> x :: rest)
    | Error e :: _ -> Error e

  let deserialize (scene : Yojson.Safe.t) world =
    let open Yojson.Safe in
    let open Json_helpers in
    let ( let* ) = Result.bind in
    let* reg_packed =
      World.get_resource world Component_registry.R.type_id
      |> Option.to_result ~none:(Error.resource_not_found "Component registry missing")
    in

    let* reg = Resource.unpack (module Component_registry.R) reg_packed in

    (* create a new world and copy across the Component registry *)
    let world = World.create () |> World.add_resource Component_registry.R.type_id reg_packed in
    let* name = parse_string "name" scene in
    let* uuid = parse_uuid "uuid" scene in
    let* version = parse_int "version" scene in
    let* entities_json = parse_list "entities" scene in
    let* resources_json = parse_list "resources" scene in

    let* entities =
      result_list_seq
      @@ List.map
           (fun e ->
             let* entity_name = parse_string "name" e in
             let* entity_uuid = parse_uuid "uuid" e in
             let* components = parse_list "components" e in

             let entity = World.add_entity ~name:entity_name ~uuid:(Some entity_uuid) world in

             let* components =
               result_list_seq
               @@ List.map
                    (fun component_json ->
                      let* component_name, component_data = parse_single_assoc component_json in

                      let* (Component { instance = (module C); serializers }) =
                        Component_registry.get_entry_by_name reg component_name
                        |> Option.to_result
                             ~none:(Error.type_register (Unregistered_component component_name))
                      in

                      let* (module S) =
                        Type_register.get_json_serializer serializers
                        |> Option.to_result
                             ~none:
                               (Error.type_register
                                  (Component_json_serializer_not_found component_name))
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
    Ok { id; uuid; name; version; entities; resources = [] }
end
