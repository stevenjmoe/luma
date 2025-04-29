module type S = sig
  open Luma__app
  open Luma__ecs
  open Luma__asset
  open Luma__render
  open Luma__image
  open Luma__sprite
  open Luma__transform
  open Luma__time
  open Luma__math
  open Luma__resource
  open Luma__id

  module App : sig
    include module type of App

    val run : t -> unit
  end

  type colour
  type texture

  module Window_config : Luma__window.Window.Window_config with type colour = colour
  module Input : Luma__input.Input.S

  module Plugin : sig
    module Config : sig
      type t = { window : Window_config.t }

      val default : unit -> t
    end

    val add_default_plugins : ?config:Config.t -> App.t -> App.t
    val camera_plugin : App.t -> App.t
    val window_plugin : ?config:Window_config.t -> App.t -> App.t
    val asset_plugin : App.t -> App.t
    val time_plugin : App.t -> App.t
    val input_plugin : App.t -> App.t
  end

  module Image : sig
    module Texture : Texture.S with type t = texture
    module Texture_atlas : module type of Texture_atlas
    module Texture_atlas_layout : module type of Texture_atlas_layout
  end

  module Colour : sig
    type t = colour

    val rgb : r:int -> g:int -> b:int -> t
    val rgba : r:int -> g:int -> b:int -> a:int -> t
    val white : t
    (* Plus any other functions your D.Colour module exposes, you must repeat them here manually if you want them visible *)
  end

  module Camera : Camera_component.S
  module Archetype : module type of Archetype
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
  module Sprite : Sprite.S with type texture = texture
  module Renderer : Render.Renderer with type texture = texture and type colour = colour
  module Key : module type of Luma__types.Key

  val screen_width : unit -> int
  val screen_height : unit -> int

  module Log : sig
    val log : ('a, Format.formatter, unit, unit) format4 -> 'a
    val error : ('a, unit) Luma__core__Log.conditional_log
    val warn : ('a, unit) Luma__core__Log.conditional_log
    val debug : ('a, unit) Luma__core__Log.conditional_log
    val info : ('a, unit) Luma__core__Log.conditional_log
  end
end

module Make : functor (D : Luma__driver.Driver.S) -> S
