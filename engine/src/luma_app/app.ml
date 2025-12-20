open Luma__ecs
open Luma__type_register
open Luma__resource
open Luma__task_queue

type t = {
  world : World.t;
  scheduler : Scheduler.t;
  plugins : (t -> t) list;
}

type placement = Scheduler.placement

let log = Luma__core.Log.sub_log "app"

let create () =
  log.info (fun log -> log "Created world");
  { world = World.create (); scheduler = Scheduler.create (); plugins = [] }

let world (app : t) = app.world
let scheduler (app : t) = app.scheduler
let add_plugin plugin app = { app with plugins = plugin :: app.plugins }
let plugins app = app.plugins
let clear_plugins app = { app with plugins = [] }

let register_component (type a) name (module C : Component.S with type t = a) serializers app =
  Type_register.Component_registry.register_component name (module C) serializers app.world;
  app

let register_resource
    (type a)
    name
    (module R : Resource.S with type t = a)
    (serializers : a Luma__serialize.Serialize.serializer_pack list)
    app =
  Type_register.Resource_registry.register_resource name (module R) serializers app.world;
  app

let on stage system ?(run_if = fun _ -> true) app =
  let uuid = System.uuid system in
  Scheduler.add_system app.scheduler stage (Scheduler.System { uuid; sys = system; run_if });
  app

let once stage system ?(placement = Scheduler.At) ?(run_if = fun _ -> true) app =
  let uuid = System.uuid system in
  let system = Scheduler.System { uuid; sys = system; run_if } in
  Scheduler.add_in_placement app.scheduler stage run_if placement system;
  app

let while_in
    (type s)
    (module S : Luma__state.State.STATE with type t = s)
    (next_state : s)
    ~stage
    ~system
    app =
  let open Luma__state.State in
  let open Luma__resource in
  let run_if world =
    match World.get_resource world State_res.R.type_id with
    | None ->
        log.error (fun l -> l "init_state must be called before scheduling a state-gated system");
        invalid_arg "on: init_state must be called before scheduling a state-gated system"
    | Some packed -> (
        match Resource.unpack (module State_res.R) packed with
        | Ok state -> (
            match State_res.current state with
            | Some c -> eq_state (State ((module S), next_state)) c
            | None -> false)
        | Error _ ->
            log.error (fun l -> l "State resource has wrong type");
            invalid_arg "on: wrong type in State_res resource.")
  in
  let uuid = System.uuid system in
  Scheduler.add_system app.scheduler stage (Scheduler.System { uuid; sys = system; run_if });
  app

let on_
    (type s)
    (module S : Luma__state.State.STATE with type t = s)
    (system : (World.t, 'a) System.t)
    (app : t)
    (pred : Luma__state__State.state_resource -> bool) =
  let open Luma__state in
  let open Luma__resource in
  let run_if world =
    match World.get_resource world State.State_res.R.type_id with
    | Some packed -> (
        match Resource.unpack_opt (module State.State_res.R) packed with
        | Some state -> pred state
        | None -> false)
    | None -> false
  in
  let uuid = System.uuid system in
  Scheduler.add_system app.scheduler StateTransition
    (Scheduler.System { uuid; sys = system; run_if });
  app

let on_enter
    (type s)
    (module S : Luma__state.State.STATE with type t = s)
    (s : s)
    (system : (World.t, 'a) System.t)
    (app : t) =
  on_ (module S) system app (Luma__state.State.just_entered (module S) s)

let on_exit
    (type s)
    (module S : Luma__state.State.STATE with type t = s)
    (s : s)
    (system : (World.t, 'a) System.t)
    (app : t) =
  on_ (module S) system app (Luma__state.State.just_exited (module S) s)

(* TODO: Clean up*)
let init_state (type a) state_mod (state : a) app =
  let open Luma__resource.Resource in
  let open Luma__state in
  let app = app |> add_plugin (fun a -> a |> on StateTransition (State.transition_system ())) in
  let packed =
    Luma__state.State.State (state_mod, state)
    |> State.State_res.create
    |> pack (module State.State_res.R)
  in
  World.add_resource State.State_res.R.type_id packed app.world |> ignore;
  app

let step app =
  let s =
    [
      Scheduler.PreUpdate;
      StateTransition;
      Update;
      PostUpdate;
      PreRender;
      Render;
      PostRender;
      Overlay;
    ]
  in
  List.iter
    (fun s ->
      Scheduler.run_stage s app.scheduler app.world |> ignore;
      ())
    s;
  app

let run_with_driver app driver =
  let ( let* ) = Lwt.bind in
  let rec loop app =
    let* app = driver app in
    let app = step app in
    loop app
  in
  loop app

let run (module D : Luma__driver.Driver.S) (app : t) =
  log.info (fun log -> log "Running applictation.");

  let rec apply_plugins app =
    match app.plugins with
    | [] -> app
    | plugins ->
        let app = { app with plugins = [] } in
        let app = List.fold_right (fun plugin app -> plugin app) plugins app in
        apply_plugins app
  in
  let app = apply_plugins app in

  let world =
    Scheduler.run_stage Scheduler.PreStartup app.scheduler app.world
    |> Scheduler.run_stage Scheduler.Startup app.scheduler
    |> Scheduler.run_stage Scheduler.PostStartup app.scheduler
  in
  let app = { app with world } in

  let rec loop app =
    if D.Window.should_close () then (
      world |> Scheduler.run_stage Cleanup app.scheduler |> ignore;
      D.Window.shutdown ())
    else (
      D.IO.run_io_loop ();
      Task_queue.Complete.apply ();
      D.Window.begin_frame ();
      let app = step app in
      D.Window.end_frame ();
      D.Window.schedule_next_frame (fun () -> loop app) |> ignore)
  in
  loop app
