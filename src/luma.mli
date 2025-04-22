module Make : functor (D : Luma__driver.Driver.S) -> sig
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

  module Window_config : Luma__window.Window.Window_config

  module Plugins : sig
    module Config : sig
      type t = { window : Window_config.t }

      val default : unit -> t
    end

    val add_default_plugins : ?config:Config.t -> App.t -> App.t
  end

  val camera_plugin : App.t -> App.t
  val window_plugin : config:Window_config.t -> App.t -> App.t
  val asset_plugin : App.t -> App.t
  val time_plugin : App.t -> App.t

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
  module Time : module type of Time
  module Transform : module type of Transform
  module World : module type of World
  module Math : module type of Luma__math
  module Sprite : module type of Sprite
  module Image : module type of Image
  module Render : module type of Render
end

(*module Make : functor (D : Luma__driver.Driver.S) -> Luma*)
