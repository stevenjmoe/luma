## 1-draw rectangle

This will demonstrate how to draw a simple rectangle to the screen which will cover creating the game world, defining some components, and adding it all to the game world.

Components need to adhere to a particular signature in order to be added to the world with the `with_component` or `add_component` functions from the `World` module. Unfortunately, this can mean a lot of repetitive boilerplate. The `component` ppx can help with this! Simple components can be defined in one line like this:

```ocaml
module Rectangle = [%component: Math.Rect.t]

```

Which is the equivalent of this:

```ocaml
module Rectangle = struct
  type t = Math.Vec2.t

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

To draw the rectangle, add the system as a `Render` system. Typically you would setup the entity in a `Startup` system, then draw it in a `Render` system.

```ocaml
let setup_rectangle () =
  let open Math in
  System.make
    ~components:Query.(End)
    (fun world entities ->
      let open World in
      let rect = Rect.create ~size:(Vec2.create 100. 100.) ~pos:(Vec2.create 20. 50.) in
      Renderer.draw_rect rect @@ Colour.rgb ~r:100 ~g:100 ~b:100;
      world |> add_entity |> with_component world (module Rectangle.C) rect |> ignore;

      world)
```

And finally running the game.

```ocaml
let () =
  App.create ()
  |> Plugin.add_default_plugins
  |> App.add_system (Scheduler.Render (Luma.System.WithoutResources (setup_rectangle ())))
  |> App.run
```
