open Luma__ecs

type t = {
  world : World.t;
  scheduler : Scheduler.t;
  plugins : (t -> t) list;
}

let create () = { world = World.create (); scheduler = Scheduler.create (); plugins = [] }
let world (app : t) = app.world
let scheduler (app : t) = app.scheduler
let add_plugin plugin app = { app with plugins = plugin :: app.plugins }

let add_system sys app =
  Scheduler.add_scheduled app.scheduler sys;
  app

let run_with_driver (type d) (module D : Luma__driver.Driver.Driver) (app : t) =
  D.init ();

  let world = Scheduler.run_stage Scheduler.Startup app.scheduler app.world in
  let app = List.fold_left (fun app plugin -> plugin app) app app.plugins in
  let app = { app with world } in

  let rec loop (world, scheduler) =
    if D.should_close () then
      D.shutdown ()
    else (
      D.begin_frame ();
      D.clear Raylib.Color.beige;
      Scheduler.run_stage PreUpdate scheduler world |> ignore;
      Scheduler.run_stage Update scheduler world |> ignore;
      Scheduler.run_stage PostUpdate scheduler world |> ignore;

      Scheduler.run_stage PreRender scheduler world |> ignore;
      Scheduler.run_stage Render scheduler world |> ignore;
      Scheduler.run_stage PostRender scheduler world |> ignore;

      D.end_frame ();
      loop (world, scheduler))
  in
  loop (app.world, app.scheduler)

let run app = run_with_driver (module Luma__driver.Driver.Raylib_driver) app
