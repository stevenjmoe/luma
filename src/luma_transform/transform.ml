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
      open Yojson

      type t = C.t

      let vec3 (v : Vec3.t) : Yojson.Safe.t =
        `Assoc [ ("x", `Float v.x); ("y", `Float v.y); ("z", `Float v.z) ]

      let to_repr transform =
        let position = vec3 transform.position in
        let scale = vec3 transform.scale in
        `Assoc
          [
            ( C.name,
              `Assoc
                [
                  ("position", position); ("scale", scale); ("rotation", `Float transform.rotation);
                ] );
          ]

      let of_repr = function `Assoc [ ("TODO", `String "TODO:") ] | _ -> Error "TODO"
    end)

let add_plugin app =
  let packed_serializer = Luma__serialize.Serialize.pack_json (module Transform_serializer) in
  App.register_component C.name (module C) [ packed_serializer ] app
