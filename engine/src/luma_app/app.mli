open Luma__ecs
open Luma__serialize
open Luma__resource
open Luma__state__State

type t
type placement = Scheduler.placement

val create : unit -> t
(** Initializes the engine with an empty world, scheduler, and plugins *)

val world : t -> World.t
(** Returns the associated world. *)

val plugins : t -> (t -> t) list
(** Returns all app plugins. *)

val scheduler : t -> Scheduler.t
(** Returns the app scheduler. *)

val clear_plugins : t -> t
(** [clear_plugins app] returns the app without any of the previously added plugins. *)

val register_component :
  string -> (module Component.S with type t = 'a) -> 'a Serialize.serializer_pack list -> t -> t
(** [register_component name component serializers app] will register component [C] with the given
    name in the application's component registry. The normalised name must be unique across all
    registered components, and will throw if it isn't. Returns the updated [App.t] *)

val register_resource :
  string -> (module Resource.S with type t = 'a) -> 'a Serialize.serializer_pack list -> t -> t
(** [register_resource name resource_module serializers app] will register resource [R] with the
    given name in the application's resource registry. The normalised name must be unique across all
    registered resources, and will throw if it isn't. Returns the updated [App.t] *)

val init_state : (module STATE with type t = 'a) -> 'a -> t -> t
(** [init_state state_module initial_state app] initialises state that can be used to control when
    systems are run. See [while_in], [on_enter], and [on_exit] for details on running systems using
    state. *)

val on : 'a. Scheduler.stage -> (World.t, 'a) System.t -> ?run_if:(World.t -> bool) -> t -> t
(** Registers a system to run during the specified scheduler stage. *)

val once :
  Scheduler.stage ->
  (World.t, 'a) System.t ->
  ?placement:placement ->
  ?run_if:(World.t -> bool) ->
  t ->
  t
(** [once stage system placement run_if app] runs the systems once in the given [stage] in the
    provided [placement].

    @param placement

    [At] : buffer in an internal per-stage queue. When the next [add_system] is called for this
    [stage], all buffered entries are flushed before that newly added [system] (FIFO). If no
    persistent system is added before the stage runs, the buffered entries are demoted to the
    stage's [Last] bucket for that run.

    [First] : System will run before any persistent system in this [stage] when ready.

    [Last] : System will run after any persistent system in this [stage] when ready.

    [Befure u] : Append to the before list keyed by anchor UUID [u]. Entries are drained immediately
    before that anchor when ready.

    [After u] : Append to the after list keyed by anchor UUID [u]. Entries are drained immediately
    after that anchor when ready.

    @param run_if The system will only run if this predicate returns true. *)

val while_in :
  (module Luma__state__State.STATE with type t = 's) ->
  's ->
  stage:Scheduler.stage ->
  system:(World.t, 'a) System.t ->
  t ->
  t
(** [while in state_module state stage system] registers a system to run during the specified
    scheduler stage, only while in the provided State. *)

val on_enter : (module STATE with type t = 's) -> 's -> (World.t, 'a) System.t -> t -> t
(** [on_enter state_module state system sched] registers a system to run once, immediately after
    transitioning into the given state. *)

val on_exit : (module STATE with type t = 's) -> 's -> (World.t, 'a) System.t -> t -> t
(** [on_exit state_module state system sched] registers a system to run once, immediately after
    transitioning from the given state. *)

val add_plugin : (t -> t) -> t -> t
(** [add_plugin plugin app] applies a plugin function to the application.

    A plugin is a function that takes an [App.t] and returns a modified [App.t]. Plugins are used to
    modularly set up initial state, register systems, add resources, spawn entities, or perform any
    other kind of app configuration before the main loop begins.

    Plugins can be composed freely and are applied in the order they are added.

    Example:
    {[
      let my_plugin app =
        let world = App.world app in
        let velocity = { x = 0.; y = 0. } in
        world
        |> World.add_entity
        |> World.with_component world (module Velocity.C) velocity
        |> ignore;
        app

      let () = App.create () |> App.add_plugin my_plugin |> App.run
    ]} *)

val step : t -> t
(** [step app] runs each system in each stage in the order they were added. *)

val run_with_driver : t -> (t -> t Lwt.t) -> unit Lwt.t

val run : (module Luma__driver.Driver.S) -> t -> unit
(** The main entry point to the engine. It sets up global resources, runs [Startup] systems, runs
    the main game loop, and runs all [Update] systems. *)
