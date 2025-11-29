module type S = sig
  open Luma__app
  open Luma__ecs
  open Luma__image
  open Luma__sprite
  open Luma__transform
  open Luma__time
  open Luma__math
  open Luma__resource
  open Luma__id
  open Luma__audio
  open Luma__type_register
  open Luma__scene
  open Luma__serialize
  open Luma__state
  open Luma__window
  open Luma__input
  open Luma__ui
  open Luma__plugin
  module Types = Luma__types

  module App : sig
    type t
    type placement = App.placement

    val create : unit -> t
    val world : t -> World.t

    val init_state : (module State.STATE with type t = 's) -> 's -> t -> t
    (** [init_state state_module initial_state app] initialises state that can be used to control
        when systems are run. See [while_in], [on_enter], and [on_exit] for details on running
        systems using state. *)

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

        [Befure u] : Append to the before list keyed by anchor UUID [u]. Entries are drained
        immediately before that anchor when ready.

        [After u] : Append to the after list keyed by anchor UUID [u]. Entries are drained
        immediately after that anchor when ready.

        @param run_if The system will only run if this predicate returns true. *)

    val register_component :
      string -> (module Component.S with type t = 'a) -> 'a Serialize.serializer_pack list -> t -> t
    (** [register_component name component serializers app] will register component [C] with the
        given name in the application's component registry. The normalised name must be unique
        across all registered components, and will throw if it isn't. Returns the updated [App.t] *)

    val register_resource :
      string -> (module Resource.S with type t = 'a) -> 'a Serialize.serializer_pack list -> t -> t
    (** [register_resource name resource_module serializers app] will register resource [R] with the
        given name in the application's resource registry. The normalised name must be unique across
        all registered resources, and will throw if it isn't. Returns the updated [App.t] *)

    val while_in :
      (module State.STATE with type t = 's) ->
      's ->
      stage:Scheduler.stage ->
      system:(World.t, 'a) System.t ->
      t ->
      t
    (** [while in state_module state stage system] registers a system to run during the specified
        scheduler stage, only while in the provided State. *)

    val on_enter : (module State.STATE with type t = 's) -> 's -> (World.t, 'a) System.t -> t -> t
    (** [on_enter state_module state system sched] registers a system to run once, immediately after
        transitioning into the given state. *)

    val on_exit : (module State.STATE with type t = 's) -> 's -> (World.t, 'a) System.t -> t -> t
    (** [on_exit state_module state system sched] registers a system to run once, immediately after
        transitioning from the given state. *)

    val add_plugin : (t -> t) -> t -> t
    (** [add_plugin plugin app] applies a plugin function to the application.

        A plugin is a function that takes an [App.t] and returns a modified [App.t]. Plugins are
        used to modularly set up initial state, register systems, add resources, spawn entities, or
        perform any other kind of app configuration before the main loop begins.

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

    val run : t -> unit
    (** [run app] The main entry point to the engine. It sets up global resources, and runs all
        systems. *)
  end

  type colour
  type texture
  type sound

  module Draw : sig
    val draw_text : string -> int -> int -> int -> colour -> unit
  end

  module Window_config : Window.Window_config with type colour = colour
  module Camera_config : module type of Luma__render.Render.Camera_config
  module Input : Input.S
  module Ui : Ui.S

  module Plugin : sig
    module Config : Plugin.Config with type window = Window_config.t

    val add_default_plugins : ?config:Config.t -> App.t -> App.t
    val window_plugin : ?config:Window_config.t -> App.t -> App.t
    val asset_plugin : App.t -> App.t
    val time_plugin : App.t -> App.t
    val input_plugin : App.t -> App.t
    val audio_plugin : App.t -> App.t
    val sprite_plugin : App.t -> App.t
    val debug_plugin : App.t -> App.t
  end

  module Image : sig
    module Texture : Texture.S with type t = texture
    module Texture_atlas : module type of Texture_atlas
    module Texture_atlas_layout : module type of Texture_atlas_layout
  end

  module Audio : Audio.S

  module Colour : sig
    type t = colour

    val rgb : r:int -> g:int -> b:int -> t
    val rgba : r:int -> g:int -> b:int -> a:int -> t
    val white : t
    val from_string : string -> (t, Luma__core.Error.error) result
  end

  open Luma__asset
  module Camera : Luma__render.Camera.S
  module Asset : module type of Asset
  module Assets : module type of Assets
  module Asset_server : module type of Server
  module Asset_loader : module type of Loader
  module Component : module type of Component
  module Id : module type of Id
  module Query : module type of Query
  module Resource : module type of Resource
  module Scheduler : module type of Scheduler
  module System : module type of System
  module Time : Time.S
  module Transform : module type of Transform
  module World : module type of World
  module Math : module type of Luma__math
  module Texture_atlas : module type of Texture_atlas
  module Texture_atlas_layout : module type of Texture_atlas_layout

  module Renderer :
    Luma__render.Render.Renderer with type texture = texture and type colour = colour

  module Sprite_plugin : Sprite.Sprite_plugin with type texture = texture
  module Sprite : Sprite.S with type texture = texture
  module Key : module type of Luma__types.Input_types.Key
  module Mouse_button : module type of Types.Input_types.Mouse_button
  module State : module type of Luma__state.State
  module Scene : Scene.S
  module IO : module type of Luma__driver.Driver.IO
  module Command : module type of Command

  val screen_width : unit -> int
  val screen_height : unit -> int

  module Log : sig
    val log : ('a, Format.formatter, unit, unit) format4 -> 'a
    val error : ('a, unit) Luma__core__Log.conditional_log
    val warn : ('a, unit) Luma__core__Log.conditional_log
    val debug : ('a, unit) Luma__core__Log.conditional_log
    val info : ('a, unit) Luma__core__Log.conditional_log
  end

  module Error : module type of Luma__core.Error
end

module Make : functor (D : Luma__driver.Driver.S) -> S
