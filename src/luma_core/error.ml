open Printf

type resource_error = {
  id : int;
  msg : string;
}

type resource =
  [ `Resource_Unpack of resource_error
  | `Resource_Not_Found of resource_error
  ]

let resource_not_found id msg = `Resource_Not_Found { id; msg }
let resource_unpack_failed id msg = `Resource_Unpack { id; msg }
let pp_resource_msg ppf fmt err = Format.fprintf ppf fmt err.msg

type t = [ | resource ]

let rec pp : 'a. _ -> ([< t ] as 'a) -> unit =
 fun ppf -> function
  | `Resource_Unpack r -> pp_resource_msg ppf "Failed to unpack resource: <%s>" r
  | `Resource_Not_Found r -> pp_resource_msg ppf "Could not find resource: <%s>" r

let show err = Print.show_of_pp pp err
