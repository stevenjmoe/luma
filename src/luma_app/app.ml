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

type state_instance =
  | State : ((module Luma__state.State.S with type t = 'a) * 'a) -> state_instance

module State_resource = struct
  include Luma__resource.Resource.Make (struct
    type inner = state_instance

    let name = "state_resource"
  end)
end

let init_state (type s) (module S : Luma__state.State.S with type t = s) (state : s) app =
  if World.get_resource app.world State_resource.type_id |> Option.is_some then (
    log.error (fun l ->
        l "State has already been initialized. Did you call `init_state` more than once?");
    invalid_arg "State has already been initialized. Did you call `init_state` more than once?")
  else
    let instance = State ((module S), state) in
    let packed = Luma__resource.Resource.pack (module State_resource) instance in
    World.add_resource State_resource.type_id packed app.world |> ignore;
    app

let matches_state
    (type s)
    (module S : Luma__state.State.S with type t = s)
    (expected_state : s)
    (State ((module R), current_state)) =
  if S.type_id = R.type_id then
    let s = R.to_base current_state in
    let s2 = S.to_base expected_state in
    s = s2
  else false

let current_state world =
  match World.get_resource world State_resource.type_id with
  | None -> None
  | Some p -> (
      match Luma__resource.Resource.unpack (module State_resource) p with
      | Ok s -> Some s
      | Error _ -> assert false)

let on
    (type s)
    ?in_state:(state : ((module Luma__state.State.S with type t = s) * s) option)
    stage
    system
    app =
  let predicate world =
    match state with
    | None -> true
    | Some ((module Q), expected_state) -> (
        match current_state world with
        | Some current_state -> matches_state (module Q) expected_state current_state
        | None -> false)
  in
  Scheduler.add_system app.scheduler stage (Scheduler.System { sys = system; run_if = predicate });
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
