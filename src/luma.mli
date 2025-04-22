module Make : functor (D : Luma__driver.Driver.S) -> sig
  module App : sig
    include module type of Luma__app.App

    val run : t -> unit
  end

  module Window_config : Luma__window.Window.Window_config

  val add_default_plugins : Luma__app__App.t -> Luma__app__App.t
  val camera_plugin : Luma__app__App.t -> Luma__app__App.t
  val window_plugin : ?config:Window_config.t -> App.t -> App.t
  val asset_plugin : Luma__app__App.t -> Luma__app__App.t
  val time_plugin : Luma__app__App.t -> Luma__app__App.t

  module Camera : Luma__render.Camera_component.S
  module Archetype : module type of Luma__ecs.Archetype
  module Asset : module type of Luma__asset.Asset
  module Assets : module type of Luma__asset.Assets
  module Asset_server : module type of Luma__asset.Server
  module Asset_loader : module type of Luma__asset.Loader
  module Component : module type of Luma__ecs.Component
  module Id : module type of Luma__id.Id
  module Query : module type of Luma__ecs.Query
  module Resource : module type of Luma__resource.Resource
  module Scheduler : module type of Luma__ecs.Scheduler
  module System : module type of Luma__ecs.System
  module Time : module type of Luma__time.Time
  module Transform : module type of Luma__transform.Transform
  module World : module type of Luma__ecs.World
  module Math : module type of Luma__math
  module Sprite : module type of Luma__sprite.Sprite
  module Image : module type of Luma__image.Image
  module Render : module type of Luma__render.Render
end

(*module Make : functor (D : Luma__driver.Driver.S) -> Luma*)
