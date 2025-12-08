open Luma__core
open Luma__serialize
open Luma__serialize.Serialize.Serialize_value

let ( let* ) = Result.bind

let get_field key ~on_missing fields =
  match List.assoc_opt key fields with Some v -> Ok v | None -> Error (on_missing key)

let get_field_opt key fields =
  match List.assoc_opt key fields with Some v -> Some v | None -> None

module Math = struct
  open Luma__math

  module Vec2 : Serialize.Codec with type t = Vec2.t = struct
    open Vec2

    type t = Vec2.t

    let to_value v = Obj [ ("x", Float v.x); ("y", Float v.y) ]

    let of_value = function
      | Obj fields ->
          let field key =
            get_field key ~on_missing:(fun k -> Error.expected_vec2 [ Field k ]) fields
          in
          let* vx = field "x" in
          let* vx = float vx in
          let* vy = field "y" in
          let* vy = float vy in
          Ok (Vec2.create vx vy)
      | _ -> Error (Error.expected_vec2 [])
  end

  module Vec3 : Serialize.Codec with type t = Vec3.t = struct
    open Vec3

    type t = Vec3.t

    let to_value v = Obj [ ("x", Float v.x); ("y", Float v.y); ("z", Float v.z) ]

    let of_value = function
      | Obj fields ->
          let field key =
            get_field key ~on_missing:(fun k -> Error.expected_vec3 [ Field k ]) fields
          in

          let* vx_v = field "x" in
          let* vx = float vx_v in
          let* vy_v = field "y" in
          let* vy = float vy_v in
          let* vz_v = field "z" in
          let* vz = float vz_v in

          Ok (Vec3.create vx vy vz)
      | _ -> Error (Error.expected_vec3 [])
  end
end

module Transform : Serialize.Codec with type t = Luma__transform.Transform.t = struct
  open Luma__transform

  type t = Transform.t

  let to_value (transform : t) : Serialize.Serialize_value.t =
    Obj
      [
        ("position", Math.Vec3.to_value transform.position);
        ("rotation", Float transform.rotation);
        ("scale", Math.Vec3.to_value transform.scale);
      ]

  let of_value (v : Serialize.Serialize_value.t) : (t, Error.error) result =
    match v with
    | Obj fields ->
        let field key =
          get_field key ~on_missing:(fun _ -> Error.expected_obj [ Field "Transform" ]) fields
        in
        let* position = field "position" in
        let* position = Math.Vec3.of_value position in
        let* rotation = field "rotation" in
        let* rotation = Serialize.Serialize_value.float rotation in
        let* scale = field "scale" in
        let* scale = Math.Vec3.of_value scale in

        Ok Transform.{ position; rotation; scale }
    | _ -> Error (Error.expected_obj [])
end

module Sprite : Serialize.Codec with type t = Luma__sprite.Sprite.spec = struct
  open Luma__sprite
  open Serialize

  type t = Sprite.spec

  let to_value (sprite : Sprite.spec) =
    let image = ("path", Serialize_value.String sprite.path) in
    let texture_atlas = ("texture_atlas", String "TODO") in
    let flip_x = ("flip_x", Bool sprite.flip_x) in
    let flip_y = ("flip_y", Bool sprite.flip_y) in
    let custom_size =
      match sprite.custom_size with
      | None -> ("custom_suze", Null)
      | Some v -> ("custom_size", Math.Vec2.to_value v)
    in
    Obj [ image; texture_atlas; flip_x; flip_y; custom_size ]

  let of_value v =
    match v with
    | Obj fields ->
        let field key =
          get_field key ~on_missing:(fun _ -> Error.expected_obj [ Field "Sprite" ]) fields
        in
        let custom_size = get_field_opt "custom_size" fields in
        let* custom_size =
          match custom_size with
          | None -> Ok None
          | Some cs -> (
              match Math.Vec2.of_value cs with Ok v -> Ok (Some v) | Error e -> Error e)
        in
        let* image_v = field "path" in
        let* path = string image_v in

        let* flip_x_v = field "flip_x" in
        let* flip_x = bool flip_x_v in

        let* flip_y_v = field "flip_y" in
        let* flip_y = bool flip_y_v in

        Ok { Sprite.path; flip_x; flip_y; custom_size; texture_atlas = None }
    | _ -> Error (Error.expected_obj [])
end

module Json = struct
  module Transform = Serialize.Make_serializer (Serialize.Json_format) (Transform)
  module Sprite_spec = Serialize.Make_serializer (Serialize.Json_format) (Sprite)
end
