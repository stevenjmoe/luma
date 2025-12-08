open Luma__serialize
open Luma__core
open Luma__math
open Rigid_body
open Json_helpers

let normalize s = s |> String.trim |> String.lowercase_ascii

let rigid_body_to_json body =
  let body_type =
    Rigid_body.body_type_to_string body.body_type |> normalize |> of_string "body_type"
  in
  let pos = of_vec2 "pos" body.pos in
  let vel = of_vec2 "vel" body.vel in
  let acc = of_vec2 "acc" body.acc in
  let force_acc = of_vec2 "force_accumulator" body.force_accumulator in
  let mass = of_float "mass" body.mass in
  let inv_mass = of_float "inv_mass" body.inv_mass in
  let damping = of_float "damping" body.damping in
  let angle = of_float "angle" body.angle in
  let active = of_bool "active" body.active in
  let shape =
    match body.shape with
    | Circle c ->
        ( "shape",
          `Assoc
            [
              of_string "type" "circle";
              of_float "radius" (Bounded2d.Bounding_circle.radius c);
              of_vec2 "center" (Bounded2d.Bounding_circle.center c);
            ] )
    | Aabb a ->
        ( "shape",
          `Assoc
            [
              ("type", `String "aabb");
              of_vec2 "min" (Bounded2d.Aabb2d.min a);
              of_vec2 "max" (Bounded2d.Aabb2d.max a);
            ] )
  in
  `Assoc [ body_type; shape; pos; vel; acc; force_acc; mass; inv_mass; damping; angle; active ]

(*module Rigid_body_serializer =
  Serialize.Make_serializer
    (Serialize.Json_format)
    (struct
      type nonrec t = t

      let to_repr body : Yojson.Safe.t = `Assoc [ (C.name, rigid_body_to_json body) ]

      let of_repr (repr : Yojson.Safe.t) =
        let ( let* ) = Result.bind in

        let json_error () = Error (Error.parse_json (Json (Yojson.Safe.pretty_to_string repr))) in

        match repr with
        | `Assoc _ as obj ->
            let* name, data = parse_single_assoc obj in
            if normalize name <> normalize C.name then json_error ()
            else
              let* body_type_str = parse_string "body_type" data in
              let body_type = body_type_str |> normalize |> Rigid_body.body_type_of_string in

              let* pos = parse_vec2 "pos" data in
              let* vel = parse_vec2 "vel" data in
              let* acc = parse_vec2 "acc" data in
              let* force_accumulator = parse_vec2 "force_accumulator" data in

              let* mass = parse_float "mass" data in
              let* inv_mass = parse_float "inv_mass" data in
              let* damping = parse_float "damping" data in
              let* angle = parse_float "angle" data in
              let* active = parse_bool "active" data in

              let shape_json = Yojson.Safe.Util.member "shape" data in
              let* shape_type = parse_string "type" shape_json in
              let shape_type = normalize shape_type in

              let* shape =
                match shape_type with
                | "circle" ->
                    let* radius = parse_float "radius" shape_json in
                    let* center = parse_vec2 "center" shape_json in
                    let c = Bounded2d.Bounding_circle.create center radius in
                    Ok (Circle c)
                | "aabb" ->
                    let* min = parse_vec2 "min" shape_json in
                    let* max = parse_vec2 "max" shape_json in
                    let a = Bounded2d.Aabb2d.of_min_max min max in
                    Ok (Aabb a)
                | _ -> json_error ()
              in

              Ok
                {
                  body_type;
                  shape;
                  pos;
                  vel;
                  acc;
                  force_accumulator;
                  mass;
                  inv_mass;
                  damping;
                  angle;
                  active;
                }
        | _ -> json_error ()
    end)

module Colliders_serializer =
  Serialize.Make_serializer
    (Serialize.Json_format)
    (struct
      open Json_helpers

      type nonrec t = t list

      let to_repr bodies : Yojson.Safe.t =
        `Assoc [ ("Colliders", `List (List.map (fun body -> rigid_body_to_json body) bodies)) ]

      let of_repr repr =
        let ( let* ) = Result.bind in
        let* colliders_json = parse_list "Colliders" repr in
        List.fold_right
          (fun j acc ->
            let* acc = acc in
            let* body = Rigid_body_serializer.deserialize j in
            Ok (body :: acc))
          colliders_json (Ok [])
    end)*)
