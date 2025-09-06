open Yojson.Safe.Util
open Luma__math
open Luma__core

let of_vec3 label (v : Vec3.t) =
  (label, `Assoc [ ("x", `Float v.x); ("y", `Float v.y); ("z", `Float v.z) ])

let of_vec2 label (v : Vec2.t) = (label, `Assoc [ ("x", `Float v.x); ("y", `Float v.y) ])
let of_float label f = (label, `Float f)
let of_bool label f = (label, `Bool f)

let parse_vec2 key json =
  match member key json with
  | `Assoc [ ("x", `Float x); ("y", `Float y) ] -> Ok (Vec2.create x y)
  | _ -> Error (Error.parse_json (Vec2 key))

let parse_vec3 key json =
  match member key json with
  | `Assoc [ ("x", `Float x); ("y", `Float y); ("z", `Float z) ] -> Ok (Vec3.create x y z)
  | _ -> Error (Error.parse_json (Vec3 key))

let parse_string key json =
  match member key json with `String v -> Ok v | _ -> Error (Error.parse_json (String key))

let parse_string_opt key json =
  match member key json with
  | `String v -> Ok (Some v)
  | `Null -> Ok None
  | _ -> Error (Error.parse_json (String key))

let parse_float key json =
  match member key json with `Float v -> Ok v | _ -> Error (Error.parse_json (Float key))

let parse_int key json =
  match member key json with `Int v -> Ok v | _ -> Error (Error.parse_json (Int key))

let parse_int_opt key json =
  match member key json with
  | `Int v -> Ok (Some v)
  | `Null -> Ok None
  | _ -> Error (Error.parse_json (Int key))

let parse_bool key json =
  match member key json with `Bool v -> Ok v | _ -> Error (Error.parse_json (Bool key))

let parse_uuid key json =
  match member key json with
  | `String v -> (
      match Uuidm.of_string v with
      | Some u -> Ok u
      | None -> Error (Error.invalid_uuid { uuid = v }))
  | _ -> Error (Error.parse_json (Uuid key))

let parse_list key json =
  match member key json with `List l -> Ok l | _ -> Error (Error.parse_json (List key))

let parse_single_assoc obj =
  match obj with
  | `Assoc [ (name, data) ] -> Ok (name, data)
  | _ -> Error (Error.parse_json (Assoc ""))

let parse_assoc key json =
  match Yojson.Safe.Util.member key json with
  | `Assoc assoc -> Ok assoc
  | _ -> Error (Error.parse_json (Assoc key))
