open Luma
module Component = Component
module Position = [%component: Raylib.Vector2.t]
module Rectangle = [%component: Raylib.Rectangle.t]

let setup_rectangle () =
  System.make
    ~components:Query.(End)
    (fun world entities ->
      let open World in
      let rect = Raylib.Rectangle.create 100. 100. 20. 50. in
      let position = Raylib.Vector2.create 140. 0. in
      let offset =
        Raylib.Vector2.create
          (Float.of_int (Raylib.get_screen_width ()) /. 2.)
          (Float.of_int (Raylib.get_screen_height ()) /. 2.)
      in
      let target = Raylib.Vector2.create (Raylib.Vector2.x position) (Raylib.Vector2.y position) in
      let camera = Raylib.Camera2D.create offset target 0. 1. in

      world
      |> add_entity
      |> with_component world (module Rectangle.C) rect
      |> with_component world (module Position.C) position
      |> with_component world (module Components.Camera.C) camera
      |> ignore;

      world)

let () =
  App.create ()
  |> App.add_system (Scheduler.Startup (Luma.System.WithoutResources (setup_rectangle ())))
  |> App.run
