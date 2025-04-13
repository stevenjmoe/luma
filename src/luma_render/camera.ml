open Luma__ecs

[%%component
module Camera = struct
  type t = Raylib.Camera2D.t

  let default () =
    Raylib.Camera2D.create (Raylib.Vector2.create 0. 0.) (Raylib.Vector2.create 0. 0.) 0. 1.
end]

let begin_camera_pass () =
  System.make
    ~components:Query.(Required (module Camera.C) & End)
    (fun world entities ->
      match entities with
      | [] -> world
      | (_, (camera, _)) :: _ ->
          Raylib.begin_mode_2d camera;
          world)

let end_camera_pass () =
  System.make
    ~components:Query.(End)
    (fun world _ ->
      Raylib.end_mode_2d ();
      world)

let plugin app =
  let world = App.world app in
  if World.query world Query.(Required (module Camera.C) & End) = [] then
    world
    |> World.add_entity
    |> World.with_component world (module Camera.C) (Camera.default ())
    |> ignore;
  app
  |> App.add_system (Update (WithoutResources (begin_camera_pass ())))
  |> App.add_system (Update (WithoutResources (end_camera_pass ())))
