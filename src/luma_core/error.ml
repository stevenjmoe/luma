type entity_not_found = {
  id : int;
  msg : string;
}

type component_not_found = {
  component_id : int;
  archetype_hash : int;
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

type error =
  [ `Entity_not_found of entity_not_found
  | `Component_not_found of component_not_found
  | `Resource_not_found of resource_not_found
  | `Unpacked_unexpected_base_type of unpacked_unexpected_base_type
  | `Unpacked_type_mismatch of unpacked_type_mismatch
  | `System_run of system_run
  ]

let pp fmt (e : error) =
  match e with
  | `Entity_not_found { id; msg } ->
      Format.fprintf fmt "Entity could not be found. Id: %d. Msg: %s" id msg
  | `Component_not_found { component_id; archetype_hash; msg } ->
      Format.fprintf fmt "Component could not be found. Id: %d. Archetype hash: %d. Msg: %s"
        component_id archetype_hash msg
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

exception Engine_error of error

let raise_error (e : error) = raise (Engine_error e)

(** [entity_not_found id msg] returns the ``Entity_not_found` error*)
let entity_not_found id msg = `Entity_not_found { id; msg }

(** [entity_not_found_exn id msg] returns Engine_error exception *)
let entity_not_found_exn id msg = raise_error @@ entity_not_found id msg

(** [component_not_found component_id archetype_hash msg] returns `Component_not_found` error*)
let component_not_found component_id hash msg =
  `Component_not_found { component_id; archetype_hash = hash; msg }

(** [component_not_found_exn component_id hash msg] returns Engine_error exception *)
let component_not_found_exn component_id hash msg =
  raise_error @@ component_not_found component_id hash msg

(** [resource_not_found msg] returns `Resource_not_found` exception*)
let resource_not_found msg = `Resource_not_found { msg }

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

(** [system_run_exn system_name msg] returns Engine_error exception *)
let system_run_exn name msg = raise_error @@ system_run name msg

let try_with f = try Ok f with Engine_error e -> Error e
let or_raise = function Ok x -> x | Error e -> raise_error e
