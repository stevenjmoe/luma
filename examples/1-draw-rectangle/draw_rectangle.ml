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

      world
      |> add_entity
      |> with_component world (module Rectangle.C) rect
      |> with_component world (module Position.C) position
      |> ignore;

      world)

let () =
  App.create ()
  |> App.add_system (Scheduler.Startup (Luma.System.WithoutResources (setup_rectangle ())))
  |> App.run
