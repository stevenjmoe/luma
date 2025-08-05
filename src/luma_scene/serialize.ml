open Luma__id
open Luma__ecs
open Luma__type_register
open Luma__resource
open Luma__serialize
open Luma__type_register.Type_register
open Types

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
            let r = `Assoc [ ("entities", `List entities) ] in
            Ok r)

  let serialize_resources scene world =
    match World.get_resource world Resource_registry.R.type_id with
    | None -> Error "TODO"
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

  let serialize (type a b) scene world : (Yojson.Safe.t, string) result =
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
        Yojson.Safe.pretty_to_channel Out_channel.stdout r;
        Ok r
    | _ -> Error "todo"
end
