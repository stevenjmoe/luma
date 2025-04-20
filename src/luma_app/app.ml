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

let run (type d) (module D : Luma__driver.Driver.S) (app : t) =
  D.Window.init ~width:1000 ~height:1000 ~title:"hello";

  let app = List.fold_left (fun app plugin -> plugin app) app app.plugins in
  let world = Scheduler.run_stage Scheduler.Startup app.scheduler app.world in
  let app = { app with world } in

  let rec loop (world, scheduler) =
    if D.Window.should_close () then
      D.Window.shutdown ()
    else (
      D.Window.begin_frame ();
      D.Window.clear D.Color.white;
      Scheduler.run_stage PreUpdate scheduler world |> ignore;
      Scheduler.run_stage Update scheduler world |> ignore;
      Scheduler.run_stage PostUpdate scheduler world |> ignore;

      Scheduler.run_stage PreRender scheduler world |> ignore;
      Scheduler.run_stage Render scheduler world |> ignore;
      Scheduler.run_stage PostRender scheduler world |> ignore;

      D.Window.end_frame ();
      loop (world, scheduler))
  in
  loop (app.world, app.scheduler)
