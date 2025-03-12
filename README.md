# Luma

Luma is an unnecessary but fun ECS based game engine written in OCaml and built upon Raylib.

**Luma is not finished.**

```ocaml
let () =
  App.create ()
  |> App.add_plugin my_plugin
  |> App.add_system (Scheduler.Startup (System.WithResources (setup_player ())))
  |> App.add_system (Scheduler.Startup (System.WithoutResources (setup_camera ())))
  |> App.add_system (Scheduler.Update (System.WithResources (input_system ())))
  |> App.run
```

