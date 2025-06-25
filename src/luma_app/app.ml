open Luma__ecs
open Type

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

type state_instance =
  | State : {
      module_ : (module Luma__state.State.S with type t = 'a);
      value : 'a;
    }
      -> state_instance

module State_resource = struct
  include Luma__resource.Resource.Make (struct
    type inner = state_instance

    let name = "state"
  end)
end

let init_state (type s) (module S : Luma__state.State.S with type t = s) (state : s) app =
  let instance : state_instance = State { module_ = (module S); value = state } in
  let packed = Luma__resource.Resource.pack (module State_resource) instance in
  World.add_resource State_resource.type_id packed app.world |> ignore;
  app

let matches_state
    (type s)
    (module S : Luma__state.State.S with type t = s)
    (expected_state : s)
    (State { module_ = (module R); value }) =
  if S.type_id = R.type_id then
    let s = R.to_base value in
    let s2 = S.to_base expected_state in
    s = s2
  else false

let current_state world =
  match World.get_resource world State_resource.type_id with
  | None -> None
  | Some p -> (
      match Luma__resource.Resource.unpack (module State_resource) p with
      | Ok s -> Some s
      | Error _ -> None)

let on
    (type s)
    ?in_state:(state : ((module Luma__state.State.S with type t = s) * s) option)
    stage
    system
    app =
  let _guard =
    match current_state app.world with
    | None -> false
    | Some cs -> (
        match state with
        | None -> false
        | Some ((module Q), value) -> matches_state (module Q) value cs)
  in
  Scheduler.add_system app.scheduler stage (Scheduler.System system);
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
    if D.Window.should_close () then (
      world |> Scheduler.run_stage Cleanup scheduler |> ignore;
      D.Window.shutdown ())
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
