open Luma__app
open Luma__ecs

(* TODO: Possibly make some or all of this plugin core behaviour. Right now the camera systems don't run if this plugin isn't added. *)
module Make (D : Luma__driver.Driver.S) (Camera : Camera_component.S with type camera = D.camera) =
struct
  let begin_camera_pass () =
    System.make
      ~components:Query.(Required (module Camera.Component.C) & End)
      (fun world entities ->
        match entities with
        | [] -> world
        | [ (_, (camera, _)) ] | (_, (camera, _)) :: _ ->
            D.Window.begin_2d camera.camera;
            world)

  let end_camera_pass () =
    System.make
      ~components:Query.(End)
      (fun world _ ->
        D.Window.end_2d ();
        world)

  let plugin app =
    let world = App.world app in
    (if World.query world Query.(Required (module Camera.Component.C) & End) = [] then
       let camera = Camera.default () in
       let c = Camera.Component.{ camera; active = true } in
       world
       |> World.add_entity
       |> World.with_component world (module Camera.Component.C) c
       |> ignore);

    app
    |> App.add_system (PreRender (WithoutResources (begin_camera_pass ())))
    |> App.add_system (PostRender (WithoutResources (end_camera_pass ())))
end
