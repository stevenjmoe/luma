(* exceptions *)
type entity_not_found = {
  id : int;
  msg : string;
}

type component_not_found = {
  component_id : int;
  archetype_hash : int;
  msg : string;
}

exception Entity_not_found of entity_not_found
exception Component_not_found of component_not_found

(** [entity_not_found id msg] returns `Entity_not_found` exception*)
let entity_not_found id msg = Entity_not_found { id; msg }

(** [component_not_found id archetype_hash msg] returns `Component_not_found` exception*)
let component_not_found component_id hash msg =
  Component_not_found { component_id; archetype_hash = hash; msg }
