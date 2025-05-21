open Luma__app.App
open Luma__driver
open Luma__render

module Make
    (D : Luma__driver.Driver.S)
    (Window : Luma__window.Window.S)
    (Camera_plugin : Camera_plugin.S)
    (Input : Luma__input.Input.S)
    (Time : Luma__time.Time.S)
    (Audio : Luma__audio.Audio.S) =
struct
  module Config = struct
    type t = { window : Window.Window_config.t }

    let default () : t = { window = Window.Window_config.default () }
  end

  let window_plugin = Window.plugin
  let time_plugin = Time.plugin
  let asset_plugin = Luma__asset.Plugin.plugin
  let camera_plugin = Camera_plugin.plugin
  let input_plugin = Input.Keyboard.plugin
  let audio_plugin = Audio.plugin
  let default_config () : Config.t = { window = Window.Window_config.default () }

  let add_default_plugins ?(config : Config.t = default_config ()) app =
    app
    |> add_plugin input_plugin
    |> add_plugin audio_plugin
    |> add_plugin (window_plugin ~config:config.window)
    |> add_plugin camera_plugin
    |> add_plugin time_plugin
    |> add_plugin asset_plugin
end
