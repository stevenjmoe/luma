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
let plugins app = app.plugins
let clear_plugins app = { app with plugins = [] }

let on
    (type s)
    ?in_state:(m : ((module Luma__state.State.STATE with type t = s) * s) option)
    stage
    system
    app =
  let run_if world =
    match m with
    | None -> true
    | Some ((module S), next_state) -> (
        match World.get_resource world Luma__state.State.State_res.R.type_id with
        | None ->
            log.error (fun l ->
                l "init_state must be called before scheduling a state-gated system");
            invalid_arg "on: init_state must be called before scheduling a state-gated system"
        | Some packed -> (
            match Luma__resource.Resource.unpack (module Luma__state.State.State_res.R) packed with
            | Ok current -> Luma__state.State.eq_state (State ((module S), next_state)) current
            | Error _ ->
                log.error (fun l -> l "State resource has wrong type");
                invalid_arg "on: wrong type in State_res resource."))
  in
  Scheduler.add_system app.scheduler stage (Scheduler.System { sys = system; run_if });
  app

let init_state (type a) state_mod state app =
  let app =
    app |> add_plugin (fun a -> a |> on StateTransition (Luma__state.State.transition_system ()))
  in
  let s = Luma__state.State.State (state_mod, state) in
  let packed = Luma__resource.Resource.pack (module Luma__state.State.State_res.R) s in
  World.add_resource Luma__state.State.State_res.R.type_id packed app.world |> ignore;
  app

let check_plugins plugins () =
  if List.length plugins = 0 then
    log.warn (fun log ->
        log
          "No plugins have been added to the application. Did you forget to call \
           `Luma.Plugin.add_default_plugins`?")

let run (module D : Luma__driver.Driver.S) (app : t) =
  log.info (fun log -> log "Running applictation.");

  (* TODO: perform this check in some other way *)
  (*check_plugins app.plugins ();*)
  let app = List.fold_right (fun plugin app -> plugin app) app.plugins app in
  let world =
    Scheduler.run_stage Scheduler.PreStartup app.scheduler app.world
    |> Scheduler.run_stage Scheduler.Startup app.scheduler
    |> Scheduler.run_stage Scheduler.PostStartup app.scheduler
  in
  let app = { app with world } in

  let rec loop (world, scheduler) =
    if D.Window.should_close () then (
      world |> Scheduler.run_stage Cleanup scheduler |> ignore;
      D.Window.shutdown ())
    else (
      D.Window.begin_frame ();

      world
      |> Scheduler.run_stage PreUpdate scheduler
      |> Scheduler.run_stage StateTransition scheduler
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
