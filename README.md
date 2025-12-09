# Luma

Luma is an unnecessary but fun ECS based game engine written in OCaml and built upon Raylib.

```ocaml
let () =
  App.create ()
  |> Plugin.add_default_plugins
  |> App.add_plugin my_plugin
  |> App.on Startup (setup_player ())
  |> App.on Startup (setup_camera ())
  |> App.on Update (input_system ())
  |> App.run
```

## Warning

Luma is unfinished and under heavy development. It's also mostly a learning project so it lacks a lot of polish, documentation, features, and tests.
