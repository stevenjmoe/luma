module Make (D : Luma__driver.Driver.S) = struct
  module Plugins = Luma__plugins.Plugins.Make (D)
  module Window_config = Plugins.Window.Window_config

  let add_default_plugins ?(config : Plugins.Config.t = Plugins.Config.default ()) app =
    Plugins.add_default_plugins ~config app

  module App = struct
    include Luma__app.App

    let run app = run (module D) app
  end

  let camera_plugin = Plugins.camera_plugin
  let window_plugin = Plugins.window_plugin
  let asset_plugin = Plugins.asset_plugin
  let time_plugin = Plugins.time_plugin

  module Archetype = Luma__ecs.Archetype
  module Asset = Luma__asset.Asset
  module Assets = Luma__asset.Assets
  module Asset_server = Luma__asset.Server
  module Asset_loader = Luma__asset.Loader
  module Camera = Plugins.C
  module Component = Luma__ecs.Component
  module Id = Luma__id.Id
  module Query = Luma__ecs.Query
  module Resource = Luma__resource.Resource
  module Scheduler = Luma__ecs.Scheduler
  module System = Luma__ecs.System
  module Time = Luma__time.Time
  module Transform = Luma__transform.Transform
  module World = Luma__ecs.World
  module Math = Luma__math
  module Sprite = Luma__sprite.Sprite
  module Image = Luma__image.Image
  module Render = Luma__render.Render
end
