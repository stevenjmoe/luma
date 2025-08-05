open Luma__math
open Luma__app
open Luma__ecs
open Luma__serialize

type t = {
  mutable position : Vec3.t;
  mutable rotation : float;
  mutable scale : Vec3.t;
}

let create ?(position = Vec3.zero ()) ?(rotation = 0.) ?(scale = Vec3.create 1.0 1.0 1.0) () =
  { position; rotation; scale }

module C = Component.Make (struct
  type inner = t

  let name = "Transform"
end)

module Transform_serializer =
  Serialize.Make_serializer
    (Serialize.Json_format)
    (struct
      open Json_helpers

      type nonrec t = t

      let to_repr transform : Yojson.Safe.t =
        let position = of_vec3 "position" transform.position in
        let scale = of_vec3 "scale" transform.scale in
        let rotation = of_float "rotation" transform.rotation in
        `Assoc [ (C.name, `Assoc [ position; scale; rotation ]) ]

      let normalize s = s |> String.trim |> String.lowercase_ascii

      let of_repr (repr : Yojson.Safe.t) =
        let ( let* ) = Result.bind in

        match repr with
        | `Assoc [ (name, data) ] when normalize name = normalize C.name ->
            let* position = parse_vec3 "position" data in
            let* scale = parse_vec3 "scale" data in
            let* rotation = parse_float "rotation" data in

            Ok (create ~position ~rotation ~scale ())
        | _ ->
            Error
              (Printf.sprintf "Invalid transform json data:\n%s"
                 (Yojson.Safe.pretty_to_string repr))
    end)

let add_plugin app =
  let packed_serializer = Luma__serialize.Serialize.pack_json (module Transform_serializer) in
  App.register_component C.name (module C) [ packed_serializer ] app
