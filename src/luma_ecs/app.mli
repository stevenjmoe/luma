type t

include module type of Resources
include module type of Components

val create : unit -> t
(** Initializes the engine with an empty world, scheduler, and plugins *)

val add_system : Scheduler.schedule -> t -> t
(** Adds either a [Startup] or [Update] system to the scheduler.

    [Startup] systems will before the main game loop and [Update] systems are run on every tick. *)

val add_plugin : (World.t -> unit) -> t -> t
(** [add_plugin plugin app] adds a plugin to the application.

    A plugin is a function that takes a [World.t] and modifies it by adding entities, components, or
    other initial state. This allows you to modularize and organize the setup of your game world.

    Example:
    {[
      let my_plugin world =
        let velocity = { x = 0.; y = 0. } in
        world
        |> World.add_entity
        |> World.with_component world (module Velocity.C) velocity
        |> ignore

      let () = App.create () |> App.add_plugin my_plugin |> App.run
    ]}*)

val run : t -> unit
(** The main entry point to the engine. It sets up global resources, runs [Startup] systems, runs
    the main game loop, and runs all [Update] systems. *)
