open Printf

type archetype_error = {
  component_id : int;
  msg : string;
  hash : int;
}

type resource_error = {
  id : int;
  msg : string;
}

type resource =
  [ `Resource_Unpack of resource_error
  | `Resource_Not_Found of resource_error
  ]

type archetype = [ `Component_Not_Found of archetype_error ]

let resource_not_found id msg = `Resource_Not_Found { id; msg }
let resource_unpack_failed id msg = `Resource_Unpack { id; msg }

let archetype_component_not_found component_id hash msg =
  `Component_Not_Found { component_id; hash; msg }

let pp_archetype_msg ppf fmt (err : archetype_error) =
  Format.fprintf ppf fmt err.msg err.component_id err.hash

let pp_resource_msg ppf fmt (err : resource_error) = Format.fprintf ppf fmt err.msg

type t =
  [ resource
  | archetype
  ]

let rec pp : 'a. _ -> ([< t ] as 'a) -> unit =
 fun ppf -> function
  | `Resource_Unpack r -> pp_resource_msg ppf "Failed to unpack resource: <%s>" r
  | `Resource_Not_Found r -> pp_resource_msg ppf "Could not find resource: <%s>" r
  | `Component_Not_Found a ->
      pp_archetype_msg ppf "%s No component with id %d could be found. Archetype hash: %d." a

let show err = Print.show_of_pp pp err
