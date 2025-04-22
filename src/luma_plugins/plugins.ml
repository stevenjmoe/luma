open Luma__app.App
open Luma__driver

module Make (D : Luma__driver.Driver.S) = struct
  module C = Luma__render.Camera_component.Make (D)
  module P = Luma__render.Camera_plugin.Make (D) (C)
  module Window = Luma__window.Window.Make (D)

  module Plugins = struct
    let window_plugin = Window.plugin
    let time_plugin = Luma__time.Time.plugin
    let asset_plugin = Luma__asset.Plugin.plugin
    let camera_plugin = P.plugin
  end

  let add_default_plugins app =
    app
    |> add_plugin Plugins.window_plugin
    |> add_plugin Plugins.camera_plugin
    |> add_plugin Plugins.time_plugin
    |> add_plugin Plugins.asset_plugin
end
