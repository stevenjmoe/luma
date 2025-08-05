open Luma__ecs
open Luma__id
open Luma__resource
open Luma__serialize.Serialize

let log = Luma__core.Log.sub_log "type_register"

module Component_registry = struct
  type 'a component_entry = {
    id : Id.Component.t;
    name : string;
    instance : (module Component.S with type t = 'a);
    serializers : 'a serializer_pack list;
  }

  type entry = Component : 'a component_entry -> entry

  type t = {
    name_to_entry : (string, entry) Hashtbl.t;
    id_to_entry : (Id.Component.t, entry) Hashtbl.t;
  }

  module R = Resource.Make (struct
    type inner = t

    let name = "component_registry"
  end)

  let create () = { name_to_entry = Hashtbl.create 16; id_to_entry = Hashtbl.create 16 }
  let normalize_name name = name |> String.trim |> String.lowercase_ascii
  let get_entry r name = Hashtbl.find_opt r.name_to_entry @@ normalize_name name

  (*TODO: proper error handling. But it should panic *)
  let register_component (type a) name (module C : Component.S with type t = a) serializers world =
    let normalized_name = normalize_name name in
    let register registry =
      match Hashtbl.find_opt registry.name_to_entry normalized_name with
      | Some e ->
          let message =
            Printf.sprintf "A component with the name %s has already been registered." name
          in
          log.error (fun l -> l "%s" message);
          failwith message
      | None ->
          let entry = Component { id = C.id; name; instance = (module C); serializers } in
          Hashtbl.add registry.name_to_entry normalized_name entry;
          Hashtbl.add registry.id_to_entry C.id entry
    in
    match World.get_resource world R.type_id with
    | None ->
        let registry = create () in
        let packed = Resource.pack (module R) registry in
        World.add_resource R.type_id packed world |> ignore;
        register registry
    | Some registry ->
        let registry = Resource.unpack (module R) registry |> Result.get_ok in
        register registry

  let get_json_serializer : type a.
      a serializer_pack list ->
      (module Serializable with type t = a and type repr = Yojson.Safe.t) option =
   fun packs ->
    let rec find = function [] -> None | Json s :: _ -> Some s in
    find packs
end
