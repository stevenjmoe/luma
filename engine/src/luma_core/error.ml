type entity_not_found = {
  id : int;
  msg : string;
}

type component_not_found = {
  component_id : int;
  msg : string;
}

type resource_not_found = { msg : string }

type unpacked_type_mismatch = {
  expected_type_id : int;
  actual_type_id : int;
  msg : string;
}

type unpacked_unexpected_base_type = {
  expected_type_id : int;
  msg : string;
}

type system_run = {
  name : string;
  msg : string;
}

type asset_load = { msg : string }
type asset_ext_unsupported = { path : string }

type parse_json =
  | String of string
  | Float of string
  | Int of string
  | Bool of string
  | Vec2 of string
  | Vec3 of string
  | Uuid of string
  | List of string
  | Assoc of string
  | Json of string

type invalid_uuid = { uuid : string }

type type_registration =
  | Unregistered_component of string
  | Unregistered_resource of string
  | Component_json_serializer_not_found of string
  | Resource_json_serializer_not_found of string

type error =
  [ `Entity_not_found of entity_not_found
  | `Component_not_found of component_not_found
  | `Resource_not_found of resource_not_found
  | `Unpacked_unexpected_base_type of unpacked_unexpected_base_type
  | `Unpacked_type_mismatch of unpacked_type_mismatch
  | `System_run of system_run
  | `Asset_load of asset_load
  | `Asset_ext_unsupported of asset_ext_unsupported
  | `Serialize of parse_json
  | `Invalid_uuid of invalid_uuid
  | `Type_registration of type_registration
  ]

let pp fmt (e : error) =
  match e with
  | `Entity_not_found { id; msg } ->
      Format.fprintf fmt "Entity could not be found. Id: %d. Msg: %s" id msg
  | `Component_not_found { component_id; msg } ->
      Format.fprintf fmt "Component could not be found. Id: %d. Msg: %s" component_id msg
  | `Resource_not_found { msg } -> Format.fprintf fmt "Resource could not be found. Msg: %s" msg
  | `Unpacked_type_mismatch { expected_type_id; actual_type_id; msg } ->
      Format.fprintf fmt
        "Provided module has a different type type id than the packed module. Expected type id: \
         %d. Actual type id: %d. Msg: %s"
        expected_type_id actual_type_id msg
  | `Unpacked_unexpected_base_type { expected_type_id; msg } ->
      Format.fprintf fmt "Packed record has an unexpected base type. Expected type id: %d. Msg: %s"
        expected_type_id msg
  | `System_run { name; msg } -> Format.fprintf fmt "Failed to run system %s. Msg: %s" name msg
  | `Asset_load { msg } -> Format.fprintf fmt "Failed to load asset. Msg: %s" msg
  | `Asset_ext_unsupported { path } ->
      Format.fprintf fmt "Incompatible asset extension. Path: %s" path
  | `Serialize (String s) -> Format.fprintf fmt "Expected string field '%s'" s
  | `Serialize (Float s) -> Format.fprintf fmt "Expected float field '%s'" s
  | `Serialize (Int s) -> Format.fprintf fmt "Expected int field '%s'" s
  | `Serialize (Bool s) -> Format.fprintf fmt "Expected bool field '%s'" s
  | `Serialize (Vec2 s) -> Format.fprintf fmt "Expected vec2 field '%s'" s
  | `Serialize (Vec3 s) -> Format.fprintf fmt "Expected vec3 field '%s'" s
  | `Serialize (Uuid s) -> Format.fprintf fmt "Expected uuid field '%s'" s
  | `Serialize (List s) -> Format.fprintf fmt "Expected list field '%s'" s
  | `Serialize (Json s) -> Format.fprintf fmt "Invalid json input:\n'%s'" s
  | `Serialize (Assoc s) ->
      Format.fprintf fmt "Expected member '%s' to be a JSON object with named fields" s
  | `Invalid_uuid { uuid } -> Format.fprintf fmt "Invalid uuid '%s'" uuid
  | `Type_registration (Unregistered_component c) ->
      Format.fprintf fmt "Component '%s' has not been registered." c
  | `Type_registration (Unregistered_resource r) ->
      Format.fprintf fmt "Resource '%s' has not been registered." r
  | `Type_registration (Component_json_serializer_not_found r) ->
      Format.fprintf fmt "Component '%s' lacks JSON serializer" r
  | `Type_registration (Resource_json_serializer_not_found r) ->
      Format.fprintf fmt "Resource '%s' lacks JSON serializer" r

exception Engine_error of error

let raise_error (e : error) = raise (Engine_error e)

(** [entity_not_found id msg] returns the ``Entity_not_found` error*)
let entity_not_found id msg = `Entity_not_found { id; msg }

(** [entity_not_found_exn id msg] returns Engine_error exception *)
let entity_not_found_exn id msg = raise_error @@ entity_not_found id msg

(** [component_not_found component_id archetype_hash msg] returns `Component_not_found` error*)
let component_not_found component_id msg = `Component_not_found { component_id; msg }

(** [component_not_found_exn component_id hash msg] returns Engine_error exception *)
let component_not_found_exn component_id msg = raise_error @@ component_not_found component_id msg

(** [resource_not_found msg] returns `Resource_not_found` exception*)
let resource_not_found msg : [> `Resource_not_found of resource_not_found ] =
  `Resource_not_found { msg }

(** [unpacked_unexpected_base_type expected_type_id msg] Returns the
    ``Unpacked_unexpected_base_type` error *)
let unpacked_unexpected_base_type expected_type_id msg =
  `Unpacked_unexpected_base_type { expected_type_id; msg }

(** [unexpected_base_type_exn expected_type_id msg] returns Engine_error exception *)
let unpacked_unexpected_base_type_exn expected_type_id msg =
  raise_error @@ unpacked_unexpected_base_type expected_type_id msg

(** [unpacked_type_mismatch expected_type_id actual_type_id msg] returns the
    ``Unpacked_type_mismatch` error *)
let unpacked_type_mismatch expected_type_id actual_type_id msg : error =
  `Unpacked_type_mismatch { expected_type_id; actual_type_id; msg }

(** [unpacked_type_mismatch_exn expected_type_id actual_type_id msg] returns Engine_error exception
*)
let unpacked_type_mismatch_exn expected_type_id actual_type_id msg =
  raise_error @@ unpacked_type_mismatch expected_type_id actual_type_id msg

(** [system_run system_name msg] returns the ``System_run` error *)
let system_run name msg : error = `System_run { name; msg }

(** [system_run_exn system_name msg] returns `Engine_error` exception *)
let system_run_exn name msg = raise_error @@ system_run name msg

(** [asset_load msg] returns the ``Asset_load` error *)
let asset_load msg : error = `Asset_load { msg }

(** [asset_load_exn msg] returns the `Engine_error` exception *)
let asset_load_exn msg = raise_error @@ asset_load msg

(** [asset_ext_unsupported ext ] returns the ``Asset_ext_unsupported` error *)
let asset_ext_unsupported path : error = `Asset_ext_unsupported { path }

(** [asset_ext_unsupported_exn] returns the `Engine_error` exception *)
let asset_ext_unsupported_exn path = raise_error @@ asset_ext_unsupported path

(** [parse_json field] returns the [`Serialize] error. Field is [parse_json] where the passed in
    string should be the field name. *)
let parse_json field : error = `Serialize field

(** [invalid_uuid uuid] returns the [`Invalid_uuid] error. *)
let invalid_uuid uuid : error = `Invalid_uuid uuid

(** [type_register type] returns the [`Type_registration] error.

    [Unregistered_component of string]: The component hasn't been registered. The string is the name
    of the component.

    [Unregistered_resource of string]: The resource hasn't been registered. The string is the name
    of the resource.

    [json_serializer_not_found of string]: A json serializer was not provided when registerting the
    component or resource. *)
let type_register t : error = `Type_registration t

let try_with f = try Ok f with Engine_error e -> Error e
let or_raise = function Ok x -> x | Error e -> raise_error e
