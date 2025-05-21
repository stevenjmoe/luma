open Luma__app
open Luma__ecs

module type S = sig
  type camera

  val plugin : App.t -> App.t
end

module Make (D : Luma__driver.Driver.S) (Camera : Camera_component.S with type camera = D.camera) :
  S with type camera = D.camera = struct
  type camera = D.camera

  let begin_camera_pass () =
    System.make
      ~components:Query.Component.(Required (module Camera.Component.C) & End)
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
        (if World.query world Query.Component.(Required (module Camera.Component.C) & End) = [] then
           let camera = Camera.default () in
           let c = Camera.Component.{ camera; active = true } in
           world
           |> World.add_entity
           |> World.with_component world (module Camera.Component.C) c
           |> ignore);
        world)

  let plugin app =
    app
    |> App.add_system (PostStartup (WithoutResources (add_camera ())))
    |> App.add_system (PreRender (WithoutResources (begin_camera_pass ())))
    |> App.add_system (PostRender (WithoutResources (end_camera_pass ())))
end
