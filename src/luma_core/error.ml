type entity_not_found = {
  id : int;
  msg : string;
}

type component_not_found = {
  component_id : int;
  archetype_hash : int;
  msg : string;
}

type asset_type_mismatch = {
  expected_type_id : int;
  actual_type_id : int;
  msg : string;
}

type unexpected_asset_base_type = {
  expected_type_id : int;
  msg : string;
}

type error =
  [ `Entity_not_found of entity_not_found
  | `Component_not_found of component_not_found
  | `Unexpected_asset_base_type of unexpected_asset_base_type
  | `Asset_type_mismatch of asset_type_mismatch
  ]

exception Engine_error of error

let raise_error (e : error) = raise (Engine_error e)

(** [entity_not_found id msg] returns the ``Entity_not_found` error*)
let entity_not_found id msg = `Entity_not_found { id; msg }

(** [entity_not_found_exn id msg] returns Enging_error exception *)
let entity_not_found_exn id msg = raise_error @@ entity_not_found id msg

(** [component_not_found component_id archetype_hash msg] returns `Component_not_found` exception*)
let component_not_found component_id hash msg =
  `Component_not_found { component_id; archetype_hash = hash; msg }

(** [component_not_found_exn component_id hash msg] returns Enging_error exception *)
let component_not_found_exn component_id hash msg =
  raise_error @@ component_not_found component_id hash msg

(** [unexpected_asset_base_type expected_type_id msg] Returns the ``Unexpected_asset_base_type`
    error *)
let unexpected_asset_base_type expected_type_id msg =
  `Unexpected_asset_base_type { expected_type_id; msg }

(** [unexpected_asset_base_type_exn expected_type_id msg] returns Enging_error exception *)
let unexpected_asset_base_type_exn expected_type_id msg =
  raise_error @@ unexpected_asset_base_type expected_type_id msg

(** [asset_type_mismatch expected_type_id actual_type_id msg] returns the ``Asset_type_mismatch`
    error *)
let asset_type_mismatch expected_type_id actual_type_id msg : error =
  `Asset_type_mismatch { expected_type_id; actual_type_id; msg }

(** [asset_type_mismatch_exn expected_type_id actual_type_id msg] returns Enging_error exception *)
let asset_type_mismatch_exn expected_type_id actual_type_id msg =
  raise_error @@ asset_type_mismatch expected_type_id actual_type_id msg

let try_with f = try Ok f with Engine_error e -> Error e
let or_raise = function Ok x -> x | Error e -> raise_error e
