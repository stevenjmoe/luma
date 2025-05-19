(* exceptions *)
type not_found = {
  type_ : string;
  msg : string;
}

exception Not_found of not_found

let not_found e = Not_found e

type archetype_error =
  [ `Component_not_found of string
  | `Archetype_not_found of string
  ]
(** Archetype module errors.

    ``Component_not_found str` : the string is expected to be the pretty printed output of the
    packed component*)

let component_not_found (msg : string) = `Component_not_found msg
let archetype_not_found (msg : string) = `Archetype_not_found

type error = [ | archetype_error ]

let pp_error fmt = function
  | `Component_not_found msg -> Format.fprintf fmt "Component could not be found: %s." msg
  | `Archetype_not_found msg -> Format.fprintf fmt "Archetype could not be found. %s" msg
