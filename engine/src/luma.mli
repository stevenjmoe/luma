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
    include module type of App

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
  module Plugin : Plugin.S with type app = App.t and type window = Window_config.t

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

  module Sprite : Sprite.S
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
