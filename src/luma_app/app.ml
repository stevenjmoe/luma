open Luma__ecs

type t = {
  world : World.t;
  scheduler : Scheduler.t;
  plugins : (t -> t) list;
}

let log = Luma__core.Log.sub_log "app"

let create () =
  log.info (fun log -> log "Created world");
  { world = World.create (); scheduler = Scheduler.create (); plugins = [] }

let world (app : t) = app.world
let scheduler (app : t) = app.scheduler
let add_plugin plugin app = { app with plugins = plugin :: app.plugins }

let add_system sys app =
  Scheduler.add_scheduled app.scheduler sys;
  app

let check_plugins plugins () =
  if List.length plugins = 0 then
    log.warn (fun log ->
        log
          "No plugins have been added to the application. Did you forget to call \
           `Luma.Plugin.add_default_plugins`?")

let run (module D : Luma__driver.Driver.S) (app : t) =
  log.info (fun log -> log "Running applictation.");

  check_plugins app.plugins ();

  let app = List.fold_right (fun plugin app -> plugin app) app.plugins app in

  let world =
    Scheduler.run_stage Scheduler.PreStartup app.scheduler app.world
    |> Scheduler.run_stage Scheduler.Startup app.scheduler
    |> Scheduler.run_stage Scheduler.PostStartup app.scheduler
  in
  let app = { app with world } in

  let rec loop (world, scheduler) =
    if D.Window.should_close () then
      D.Window.shutdown ()
    else (
      D.Window.begin_frame ();
      world
      |> Scheduler.run_stage PreUpdate scheduler
      |> Scheduler.run_stage Update scheduler
      |> Scheduler.run_stage PostUpdate scheduler
      |> Scheduler.run_stage PreRender scheduler
      |> Scheduler.run_stage Render scheduler
      |> Scheduler.run_stage PostRender scheduler
      |> ignore;

      D.Window.end_frame ();
      loop (world, scheduler))
  in
  loop (app.world, app.scheduler)
