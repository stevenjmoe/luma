open Luma__app.App
open Luma__driver
open Luma__type_register
open Luma__transform
open Luma__image
open Luma__scene
open Luma__render
open Luma__window
open Luma__input
open Luma__time
open Luma__audio
open Luma__sprite
open Luma__debug

(** Engine configuration shared by the core plugins. *)
module type Config = sig
  type t

  type window
  (** Window configuration used by the window plugin. *)

  val default : unit -> t
  (** [default ()] returns a configuration suitable for most examples and simple applications. *)

  val window : t -> window
  (** [window cfg] extracts the window configuration from [cfg]. *)

  val camera : t -> Render.Camera_config.t
  (** [camera cfg] returns the default camera configuration used by the render plugin when [cfg] is
      passed to [add_default_plugins]. *)

  val create : ?window:window -> ?camera:Render.Camera_config.t -> unit -> t
  (** [create ?window ?camera ()] builds a configuration from optional window and camera settings,
      falling back to sensible defaults when omitted. *)
end

(** Core engine plugins. Each plugin is a pure transformation from [app] to [app] that registers
    systems, resources and types required by a specific subsystem. *)
module type S = sig
  type window
  (** Window configuration type expected by [window_plugin]. *)

  type config
  (** Engine configuration type, as produced by [Config]. *)

  type app
  (** Application type the plugins operate on. *)

  module Config : Config with type window = window and type t = config

  val asset_plugin : app -> app
  (** [asset_plugin app] installs the asset system: it registers asset resources, the asset server
      and loader hooks. *)

  val window_plugin : ?config:window -> app -> app
  (** [window_plugin ?config app] initialises the window using [config] (or a driver-specific
      default when omitted) and registers the core window systems such as clearing the backbuffer
      each frame. *)

  val time_plugin : app -> app
  (** [time_plugin app] installs the time subsystem: a time resource, serializers, and an update
      system that advances the simulation clock every frame. *)

  val input_plugin : app -> app
  (** [input_plugin app] installs the input subsystem for the current driver (keyboard, mouse,
      gamepads, …). *)

  val audio_plugin : app -> app
  (** [audio_plugin app] initialises the audio device, registers audio resources and systems
      responsible for updating streams and cleaning up sounds/music on shutdown. *)

  val debug_plugin : app -> app
  (** [debug_plugin app] installs debug visualisation: basic GUI panels for inspecting entities,
      components and other engine state. *)

  val texture_plugin : app -> app
  (** [texture_plugin app] registers texture asset types and file loaders so textures can be
      requested through the asset server. *)

  val scene_plugin : app -> app
  (** [scene_plugin app] registers scene asset types and file loaders so worlds can be saved/loaded
      as scenes. *)

  val render_plugin : ?camera_config:Render.Camera_config.t -> app -> app
  (** [render_plugin ?camera_config app] installs the render pipeline: render queue resources,
      per-frame setup systems, and extract systems that pull sprites/shapes into the render queue.
      If [camera_config] is omitted, the default camera from [Config] is used when building a
      default config. *)

  val serializers_plugin : app -> app
  (** [serializers_plugin app] adds core engine component/resource serializers to the app. *)

  val type_register_plugin : app -> app
  (** [type_register_plugin app] ensures the component and resource type registries have been
      created and added to the world. It won't recreate them if they already exist. *)

  val default_config : unit -> config
  (** [default_config ()] builds an engine configuration that matches the defaults used by
      [add_default_plugins]. *)

  val add_default_plugins : ?config:config -> app -> app
  (** [add_default_plugins ?config app] installs the engine’s standard plugin stack (window, time,
      input, audio, assets, transforms, sprites, scenes, rendering, etc.).

      The optional [config] controls window and camera setup; when omitted, [default_config ()] is
      used.

      Call order matters: any plugins already registered on [app] will run before the defaults, and
      plugins added after [add_default_plugins] will run later in the pipeline. *)
end

module Make : functor
  (D : Luma__driver.Driver.S)
  (Window : Luma__window.Window.S)
  (Renderer : Luma__render.Render.Renderer)
  (Input : Luma__input.Input.S)
  (Time : Luma__time.Time.PLUGIN)
  (Audio : Luma__audio.Audio.S)
  (Texture : Luma__image.Texture.S)
  (Scene : Luma__scene.Scene.S)
  (Debug : Luma__debug.Debug.S)
  -> S with type window = Window.Window_config.t and type app = Luma__app.App.t
