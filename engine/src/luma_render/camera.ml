open Luma__ecs
open Luma__math
open Luma__app
module Viewport = Viewport

type t = {
  mutable target : Vec2.t;
  mutable offset : Vec2.t;
  mutable rotation : float;
  mutable zoom : float;
  mutable viewport : Viewport.t option;
  mutable order : int;
  mutable active : bool;
}

module C = Component.Make (struct
  type inner = t

  let name = "Camera"
end)

let default () =
  {
    target = Vec2.create 0. 0.;
    offset = Vec2.create 0. 0.;
    rotation = 0.;
    zoom = 1.;
    viewport = None;
    order = 0;
    active = true;
  }

let make ?viewport ?(order = 0) ~offset ~target ~rotation ~zoom () =
  { viewport; active = true; order; target; offset; rotation; zoom }

let target c = c.target
let offset c = c.offset
let zoom c = c.zoom
let rotation c = c.rotation
let viewport c = c.viewport
let order c = c.order
let active c = c.active
let set_target c target = c.target <- target
let set_offset c offset = c.offset <- offset
let set_zoom c zoom = c.zoom <- zoom
let set_rotation c rotation = c.rotation <- rotation
let set_order c order = c.order <- order
let set_active c active = c.active <- active
let set_viewport c viewport = c.viewport <- viewport

let add_camera default_camera () =
  System.make ~components:End "add_camera" (fun world _ _ ->
      if default_camera then (
        let camera = default () in
        world
        |> World.add_entity ~name:"Camera"
        |> World.with_component world (module C) camera
        |> ignore;
        world)
      else world)

(*module Camera_serializer =
    Serialize.Make_serializer
      (Serialize.Json_format)
      (struct
        open Json_helpers

        type nonrec t = t

        let normalize s = s |> String.trim |> String.lowercase_ascii

        let to_repr camera =
          let target = of_vec2 "target" @@ target camera in
          let offset = of_vec2 "offset" @@ offset camera in
          let zoom = of_float "zoom" (zoom camera) in
          let rotation = of_float "rotation" (rotation camera) in
          let active = of_bool "active" camera.active in
          `Assoc [ (C.name, `Assoc [ target; offset; zoom; rotation; active ]) ]

        let of_repr repr =
          let ( let* ) = Result.bind in
          match repr with
          | `Assoc [ (name, data) ] when normalize name = normalize C.name ->
              let* target = parse_vec2 "target" data in
              let* offset = parse_vec2 "offset" data in
              let* zoom = parse_float "zoom" data in
              let* rotation = parse_float "rotation" data in
              let* active = parse_bool "active" data in

              Ok (make ~offset ~target ~zoom ~rotation ())
          | _ -> Error (Error.parse_json (Json (Yojson.Safe.pretty_to_string repr)))
      end)

  let register_component app =
    let packed_serializer = Luma__serialize.Serialize.pack_json (module Camera_serializer) in
    App.register_component C.name (module C) [ packed_serializer ] app*)

let plugin default_camera app =
  app
  |>
  (*register_component |>*)
  App.on PostStartup (add_camera default_camera ())
