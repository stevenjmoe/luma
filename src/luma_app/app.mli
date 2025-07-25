open Luma__ecs

type t

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

val init_state : (module Luma__state__State.STATE with type t = 'a) -> 'a -> t -> t

val on : 'a 'b. Scheduler.stage -> (World.t, 'b) System.t -> t -> t
(** Registers a system to run during the specified scheduler stage. *)

val while_in :
  (module Luma__state__State.STATE with type t = 's) ->
  's ->
  stage:Scheduler.stage ->
  system:(World.t, 'a) System.t ->
  t ->
  t
(** [while in state_module state stage system] registers a system to run during the specified
    scheduler stage, only while in the provided State. *)

val on_enter :
  (module Luma__state__State.STATE with type t = 's) -> 's -> (World.t, 'a) System.t -> t -> t
(** [on_enter state_module state_value system sched] registers a system to run once, immediately
    after transitioning into the given state. *)

val on_exit :
  (module Luma__state__State.STATE with type t = 's) -> 's -> (World.t, 'a) System.t -> t -> t
(** [on_exit state_module state_value system sched] registers a system to run once, immediately
    after transitioning from the given state. *)

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

val run : (module Luma__driver.Driver.S) -> t -> unit
(** The main entry point to the engine. It sets up global resources, runs [Startup] systems, runs
    the main game loop, and runs all [Update] systems. *)
