open Luma__driver
open Luma__app
open Luma__ecs

[%%component
module Camera = struct
  type t = {
    camera : Raylib.Camera2D.t;
    active : bool;
  }

  let default () =
    let offset =
      Raylib.Vector2.create
        (Float.of_int (Raylib.get_screen_width ()) /. 2.)
        (Float.of_int (Raylib.get_screen_height ()) /. 2.)
    in
    { camera = Raylib.Camera2D.create offset (Raylib.Vector2.create 5. 5.) 0. 1.; active = false }
end]

(* TODO: render phases per camera. *)
let begin_camera_pass (module D : Luma__driver.Driver.Driver) =
  System.make
    ~components:Query.(Required (module Camera.C) & End)
    (fun world entities ->
      match entities with
      | [] -> world
      (* For now get the first camera in the list. Later I would like to handle the above todo *)
      | [ (_, (camera, _)) ] | (_, (camera, _)) :: _ ->
          let open Camera in
          D.begin_2d camera.camera;
          world)

let end_camera_pass (module D : Luma__driver.Driver.Driver) =
  System.make
    ~components:Query.(End)
    (fun world _ ->
      D.end_2d ();
      world)

(* TODO: Possibly make some or all of this plugin core behaviour. Right now the camera systems don't run if this plugin isn't added. *)
let plugin (module D : Driver.Driver) app =
  let world = App.world app in
  if World.query world Query.(Required (module Camera.C) & End) = [] then
    world
    |> World.add_entity
    |> World.with_component world (module Camera.C) (Camera.default ())
    |> ignore;

  app
  |> App.add_system (PreRender (WithoutResources (begin_camera_pass (module D))))
  |> App.add_system (PostRender (WithoutResources (end_camera_pass (module D))))
