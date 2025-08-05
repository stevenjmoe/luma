open Yojson.Safe.Util
open Luma__math

let of_vec3 label (v : Vec3.t) =
  (label, `Assoc [ ("x", `Float v.x); ("y", `Float v.y); ("z", `Float v.z) ])

let of_vec2 label (v : Vec2.t) = (label, `Assoc [ ("x", `Float v.x); ("y", `Float v.y) ])
let of_float label f = (label, `Float f)
let of_bool label f = (label, `Bool f)

let parse_vec2 key json =
  match member key json with
  | `Assoc [ ("x", `Float x); ("y", `Float y) ] -> Ok (Vec2.create x y)
  | _ ->
      Yojson.Safe.pretty_to_channel Out_channel.stdout json;
      Error "Expected a vec2"

let parse_vec3 key json =
  match member key json with
  | `Assoc [ ("x", `Float x); ("y", `Float y); ("z", `Float z) ] -> Ok (Vec3.create x y z)
  | _ ->
      Yojson.Safe.pretty_to_channel Out_channel.stdout json;
      Error "Expected a vec3"

let parse_string key json =
  match member key json with
  | `String v -> Ok v
  | _ -> Error (Printf.sprintf "Expected string field '%s'" key)

let parse_float key json =
  match member key json with
  | `Float v -> Ok v
  | _ -> Error (Printf.sprintf "Expected float field '%s'" key)

let parse_uuid key json =
  match member key json with
  | `String v -> (
      match Uuidm.of_string v with
      | Some u -> Ok u
      | None -> Error (Printf.sprintf "Invalid uuid string in field '%s'" key))
  | _ -> Error (Printf.sprintf "Expected uuid string field '%s'" key)

let parse_list key json =
  match member key json with
  | `List l -> Ok l
  | _ -> Error (Printf.sprintf "Expected list field '%s'" key)

let extract_single_assoc obj =
  match obj with
  | `Assoc [ (name, data) ] -> Ok (name, data)
  | _ -> Error "Each assoc entry must be a single-field object."

let parse_assoc key json =
  match Yojson.Safe.Util.member key json with
  | `Assoc assoc -> Ok assoc
  | _ -> Error (Printf.sprintf "Expected member '%s' to be a JSON object with named fields" key)
