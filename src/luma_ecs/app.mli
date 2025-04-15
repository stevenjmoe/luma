type t

val create : unit -> t
(** Initializes the engine with an empty world, scheduler, and plugins *)

val world : t -> World.t
(** Returns the associated world. *)

val scheduler : t -> Scheduler.t
val add_system : Scheduler.scheduled -> t -> t

(** Adds either a [Startup] or [Update] system to the scheduler.

    [Startup] systems will before the main game loop and [Update] systems are run on every tick. *)

module type Driver = sig
  val init : unit -> unit
  val shutdown : unit -> unit
  val should_close : unit -> bool
  val get_frame_time : unit -> float
  val begin_frame : unit -> unit
  val end_frame : unit -> unit
  val begin_2d : Raylib.Camera2D.t -> unit
  val end_2d : unit -> unit
  val clear : Raylib.Color.t -> unit
end

module Raylib_driver : Driver

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

val run : t -> unit
(** The main entry point to the engine. It sets up global resources, runs [Startup] systems, runs
    the main game loop, and runs all [Update] systems. *)
