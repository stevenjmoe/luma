open Luma__serialize
open Luma__core
open Luma__math
open Rigid_body

let ( let* ) = Result.bind
let normalize s = s |> String.trim |> String.lowercase_ascii

let rigid_body_to_value rb =
  let open Serialize.Serialize_value in
  let open Luma__codecs in
  let body_type = Rigid_body.body_type_to_string rb.body_type |> normalize in
  let body_type = ("body_type", String body_type) in
  let pos = ("pos", Codecs.Math.Vec2.to_value rb.pos) in
  let vel = ("vel", Codecs.Math.Vec2.to_value rb.vel) in
  let acc = ("acc", Codecs.Math.Vec2.to_value rb.acc) in
  let force_accumulator = ("force_accumulator", Codecs.Math.Vec2.to_value rb.force_accumulator) in
  let mass = ("mass", Float rb.mass) in
  let inv_mass = ("inv_mass", Float rb.inv_mass) in
  let damping = ("damping", Float rb.damping) in
  let angle = ("angle", Float rb.angle) in
  let active = ("active", Bool rb.active) in
  let shape =
    match rb.shape with
    | Circle c ->
        ( "shape",
          Obj
            [
              ("type", String "circle");
              ("radius", Float (Bounded2d.Bounding_circle.radius c));
              ("center", Codecs.Math.Vec2.to_value (Bounded2d.Bounding_circle.center c));
            ] )
    | Aabb a ->
        ( "shape",
          Obj
            [
              ("type", String "aabb");
              ("min", Codecs.Math.Vec2.to_value (Bounded2d.Aabb2d.min a));
              ("max", Codecs.Math.Vec2.to_value (Bounded2d.Aabb2d.max a));
            ] )
  in

  Obj [ body_type; pos; vel; acc; force_accumulator; mass; inv_mass; damping; angle; active; shape ]

module Codecs = struct
  open Luma__codecs

  module Rigid_body : Luma__serialize.Serialize.Codec with type t = t = struct
    type nonrec t = t

    let of_value_inner fields =
      let open Serialize.Serialize_value in
      let open Codecs in
      let* body_type_v =
        get_field "body_type" ~on_missing:(fun s -> Error.expected_string [ Field s ]) fields
      in
      let* body_type = string body_type_v in
      let body_type = Rigid_body.body_type_of_string body_type in

      let* pos_v =
        get_field "pos" ~on_missing:(fun s -> Error.expected_string [ Field s ]) fields
      in
      let* pos = Codecs.Math.Vec2.of_value pos_v in

      let* vel_v =
        get_field "vel" ~on_missing:(fun s -> Error.expected_string [ Field s ]) fields
      in
      let* vel = Codecs.Math.Vec2.of_value vel_v in

      let* acc_v =
        get_field "acc" ~on_missing:(fun s -> Error.expected_string [ Field s ]) fields
      in
      let* acc = Codecs.Math.Vec2.of_value acc_v in

      let* force_acc_v =
        get_field "force_accumulator"
          ~on_missing:(fun s -> Error.expected_string [ Field s ])
          fields
      in
      let* force_acc = Codecs.Math.Vec2.of_value force_acc_v in

      let* mass_v =
        get_field "mass" ~on_missing:(fun s -> Error.expected_string [ Field s ]) fields
      in
      let* mass = float mass_v in

      let* inv_mass_v =
        get_field "inv_mass" ~on_missing:(fun s -> Error.expected_string [ Field s ]) fields
      in
      let* inv_mass = float inv_mass_v in

      let* damping_v =
        get_field "damping" ~on_missing:(fun s -> Error.expected_string [ Field s ]) fields
      in
      let* damping = float damping_v in

      let* angle_v =
        get_field "angle" ~on_missing:(fun s -> Error.expected_string [ Field s ]) fields
      in
      let* angle = float angle_v in

      let* active_v =
        get_field "active" ~on_missing:(fun s -> Error.expected_string [ Field s ]) fields
      in
      let* active = bool active_v in

      let* shape_v =
        get_field "shape" ~on_missing:(fun s -> Error.expected_string [ Field s ]) fields
      in
      let* shape =
        match shape_v with
        | Obj shape_fields -> (
            let* shape_type_v =
              get_field "type" ~on_missing:(fun s -> Error.expected_string [ Field s ]) shape_fields
            in
            let* shape_type = string shape_type_v in
            match normalize shape_type with
            | "circle" ->
                let* radius_v =
                  get_field "radius"
                    ~on_missing:(fun s -> Error.expected_float [ Field s ])
                    shape_fields
                in
                let* radius = float radius_v in

                let* center =
                  get_field "center"
                    ~on_missing:(fun s -> Error.expected_float [ Field s ])
                    shape_fields
                in
                let* center = Math.Vec2.of_value center in
                let c = Bounded2d.Bounding_circle.create center radius in

                Ok (Circle c)
            | "aabb" ->
                let* min_v =
                  get_field "min"
                    ~on_missing:(fun s -> Error.expected_float [ Field s ])
                    shape_fields
                in
                let* min = Math.Vec2.of_value min_v in

                let* max_v =
                  get_field "max"
                    ~on_missing:(fun s -> Error.expected_float [ Field s ])
                    shape_fields
                in

                let* max = Math.Vec2.of_value max_v in
                let a = Bounded2d.Aabb2d.of_min_max min max in
                Ok (Aabb a)
            | _ -> Error (Error.expected_obj [ Field "shape" ]))
        | _ -> Error (Error.expected_obj [ Field "shape" ])
      in
      Ok
        {
          body_type;
          shape;
          pos;
          vel;
          acc;
          force_accumulator = force_acc;
          mass;
          inv_mass;
          damping;
          angle;
          active;
        }

    let to_value rb = Serialize.Serialize_value.Obj [ (C.name, rigid_body_to_value rb) ]

    let of_value v =
      let open Serialize.Serialize_value in
      let open Codecs in
      match v with
      | Obj [ (name, Obj fields) ] when normalize name = normalize C.name -> of_value_inner fields
      | Obj fields -> of_value_inner fields
      | _ -> Error (Error.expected_obj [ Field "Rigid_body" ])
  end

  module Colliders : Serialize.Codec with type t = Colliders.t = struct
    type nonrec t = Colliders.t

    let to_value bodies =
      let open Serialize.Serialize_value in
      Obj [ ("colliders", List (List.map (fun body -> rigid_body_to_value body) bodies)) ]

    let of_value v =
      let open Serialize.Serialize_value in
      let decode_list bodies =
        List.fold_right
          (fun v acc ->
            let* acc = acc in
            let* body = Rigid_body.of_value v in
            Ok (body :: acc))
          bodies (Ok [])
      in
      match v with
      | List bodies -> decode_list bodies
      | Obj [ (name, List bodies) ] when normalize name = "colliders" -> decode_list bodies
      | _ -> Error (Error.expected_list [ Field "Colliders" ])
  end
end

module Rigid_body_serializer = Serialize.Make_serializer (Serialize.Json_format) (Codecs.Rigid_body)
module Colliders_serializer = Serialize.Make_serializer (Serialize.Json_format) (Codecs.Colliders)
