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

        type t = Camera.Camera.t

        let make_vec2 vec2 =
          `Assoc [ ("x", `Float (Luma__math.Vec2.x vec2)); ("y", `Float (Luma__math.Vec2.y vec2)) ]

        let to_repr camera =
          let target = ("target", make_vec2 @@ Camera.target camera.camera) in
          let offset = ("offset", make_vec2 @@ Camera.offset camera.camera) in
          let zoom = ("zoom", `Float (Camera.zoom camera.camera)) in
          let rotation = ("rotation", `Float (Camera.rotation camera.camera)) in
          let active = ("active", `Bool camera.active) in
          `Assoc [ (C.name, `Assoc [ target; offset; zoom; rotation; active ]) ]

        (*TODO: acual deserialize *)
        let of_repr repr = Ok { camera = Camera.default (); active = true }
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
