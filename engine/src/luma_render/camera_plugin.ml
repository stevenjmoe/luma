open Luma__app
open Luma__ecs
open Luma__serialize

module type S = sig
  type camera

  val plugin : App.t -> App.t
end

module Make (D : Luma__driver.Driver.S) (Camera : Camera_component.S with type camera = D.camera) :
  S with type camera = D.camera = struct
  type camera = D.camera

  let begin_camera_pass () =
    System.make
      ~components:Query.Component.(Required (module Camera.Camera.C) & End)
      "begin_camera_pass"
      (fun world entities ->
        match List.rev entities with
        | [] -> world
        | [ (_, (camera, _)) ] | (_, (camera, _)) :: _ ->
            D.Window.begin_2d camera.camera;
            world)

  let end_camera_pass () =
    System.make
      ~components:Query.(End)
      "end_camera_pass"
      (fun world _ ->
        D.Window.end_2d ();
        world)

  let add_camera () =
    System.make ~components:End "add_camera" (fun world entities ->
        (if World.query world Query.Component.(Required (module Camera.Camera.C) & End) = [] then
           let camera = Camera.default () in
           let c = Camera.Camera.{ camera; active = true } in
           world
           |> World.add_entity ~name:"Camera"
           |> World.with_component world (module Camera.Camera.C) c
           |> ignore);
        world)

  module Camera_serializer =
    Serialize.Make_serializer
      (Serialize.Json_format)
      (struct
        open Camera.Camera
        open Json_helpers

        type t = Camera.Camera.t

        let normalize s = s |> String.trim |> String.lowercase_ascii

        let to_repr camera =
          let target = of_vec2 "target" @@ Camera.target camera.camera in
          let offset = of_vec2 "offset" @@ Camera.offset camera.camera in
          let zoom = of_float "zoom" (Camera.zoom camera.camera) in
          let rotation = of_float "rotation" (Camera.rotation camera.camera) in
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

              Ok { camera = Camera.make ~offset ~target ~zoom ~rotation (); active }
          | _ ->
              Error
                (Printf.sprintf "Invalid camera json data:\n%s" (Yojson.Safe.pretty_to_string repr))
      end)

  let register_component app =
    let packed_serializer = Luma__serialize.Serialize.pack_json (module Camera_serializer) in
    App.register_component Camera.Camera.C.name (module Camera.Camera.C) [ packed_serializer ] app

  let plugin app =
    app
    |> register_component
    |> App.on PostStartup (add_camera ())
    |> App.on PreRender (begin_camera_pass ())
    |> App.on PostRender (end_camera_pass ())
end
