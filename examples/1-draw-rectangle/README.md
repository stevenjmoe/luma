## 1-draw rectangle

This will demonstrate how to draw a simple rectangle to the screen which will cover creating the game world, defining some components, and adding it all to the game world.

Components need to adhere to a particular signature in order to be added to the world with the `with_component` or `add_component` functions from the `World` module. Unfortunately, this can mean a lot of repetitive boilerplate. The `component` ppx can help with this! Simple components can be defined in one line like this:

```ocaml
module Position = [%component: Raylib.Vector2.t]

```

Which is the equivalent of this:

```ocaml
module Position = struct
  type t = Raylib.Vector2.t

  module C = Component.Make (struct
    type inner = t
  end)
end
```

The dune file should include the preprocess stanza in order for this to work

```dune
(executable
  (name rectangle)
  (libraries luma)
  (preprocess
    (pps ppx_component)))
```

At the moment the Component module needs to be aliased at the top `module Component = Luma.Component`.

The rectangle entity and its components can be added to the world through a `Startup` system.

```ocaml
let setup_rectangle () =
  Luma.System.make
    Luma.Query.(End)
    (fun world entities ->
      let open Luma.World in

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
      |> with_component world (module Luma.Camera.C) camera
      |> ignore;

      world)
```

And finally running the game.

```ocaml
let () =
  Luma.create ()
  |> Luma.add_system (Luma.Scheduler.Startup (Luma.System.WithoutResources (setup_rectangle ())))
  |> Luma.run
```

Currently one `Camera` module is required otherwise the application will crash, but that will probably change.
