open Luma__ecs
open Luma__id
open Luma__resource

let log = Luma__core.Log.sub_log "type_register"

module Component_registry = struct
  type 'a component_entry = {
    id : Id.Component.t;
    name : string;
    instance : (module Component.S with type t = 'a);
  }

  type entry = Component : 'a component_entry -> entry

  type t = {
    name_to_entry : (string, entry) Hashtbl.t;
    id_to_entry : (Id.Component.t, entry) Hashtbl.t;
  }

  let create () = { name_to_entry = Hashtbl.create 16; id_to_entry = Hashtbl.create 16 }

  module R = Resource.Make (struct
    type inner = t

    let name = "component_registry"
  end)

  let normalize_name name = name |> String.trim |> String.lowercase_ascii

  (*TODO: proper error handling. But it should panic *)
  let register_component (type a) name (module C : Component.S with type t = a) world =
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
          let entry = Component { id = C.id; name; instance = (module C) } in
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
end
