module Make (D : Luma__driver.Driver.S) = struct
  (* make required modules *)
  module Window = Luma__window.Window.Make (D)
  module Camera_component = Luma__render.Camera_component.Make (D)
  module Camera_plugin = Luma__render.Camera_plugin.Make (D) (Camera_component)
  module Plugins = Luma__plugins.Plugins.Make (D) (Window) (Camera_plugin)
  module Window_config = Window.Window_config

  let add_default_plugins ?(config : Plugins.Config.t = Plugins.Config.default ()) app =
    Plugins.add_default_plugins ~config app

  module App = struct
    include Luma__app.App

    let run app = run (module D) app
  end

  (* plugins *)
  let camera_plugin = Plugins.camera_plugin
  let window_plugin = Plugins.window_plugin
  let asset_plugin = Plugins.asset_plugin
  let time_plugin = Plugins.time_plugin

  type colour = Window_config.colour

  module Colour = struct
    type t = colour

    include D.Colour
  end

  module Archetype = Luma__ecs.Archetype
  module Asset = Luma__asset.Asset
  module Assets = Luma__asset.Assets
  module Asset_server = Luma__asset.Server
  module Asset_loader = Luma__asset.Loader
  module Camera = Camera_component
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
