open Luma__app.App
open Luma__driver

module Make (D : Luma__driver.Driver.S) = struct
  module C = Luma__render.Camera_component.Make (D)
  module P = Luma__render.Camera_plugin.Make (D) (C)
  module Window = Luma__window.Window.Make (D)

  module Config = struct
    type t = { window : Window.Window_config.t }

    let default () : t = { window = Window.Window_config.default () }
  end

  let window_plugin = Window.plugin
  let time_plugin = Luma__time.Time.plugin
  let asset_plugin = Luma__asset.Plugin.plugin
  let camera_plugin = P.plugin
  let default_config () : Config.t = { window = Window.Window_config.default () }

  let add_default_plugins ?(config : Config.t = default_config ()) app =
    app
    |> add_plugin (window_plugin ~config:config.window)
    |> add_plugin camera_plugin
    |> add_plugin time_plugin
    |> add_plugin asset_plugin
end
