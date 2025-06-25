open Luma__ecs

type t

module State_resource : sig
  type t

  val type_id : Luma__id__Id.Resource.t
  val name : string
  val pp : t Fmt.t
  val of_base : Luma__resource__Resource.base -> t
  val of_base_opt : Luma__resource__Resource.base -> t option
  val to_base : t -> Luma__resource__Resource.base
end

val create : unit -> t
(** Initializes the engine with an empty world, scheduler, and plugins *)

val world : t -> World.t
(** Returns the associated world. *)

val scheduler : t -> Scheduler.t
val init_state : (module Luma__state__State.S with type t = 's) -> 's -> t -> t

val on :
  ?in_state:(module Luma__state__State.S with type t = 's) * 's ->
  Scheduler.stage ->
  (World.t, 'a) System.t ->
  t ->
  t
(** Registers a system to run during the specified scheduler stage. *)

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
