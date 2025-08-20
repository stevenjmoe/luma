open Luma__app.App
open Luma__driver
open Luma__render
open Luma__type_register
open Luma__transform
open Luma__image

module Make
    (D : Luma__driver.Driver.S)
    (Window : Luma__window.Window.S)
    (Camera_plugin : Camera_plugin.S)
    (Input : Luma__input.Input.S)
    (Time : Luma__time.Time.S)
    (Audio : Luma__audio.Audio.S)
    (Sprite_plugin : Luma__sprite.Sprite.Sprite_plugin)
    (Texture : Texture.S)
    (Debug : Luma__debug.Debug.S) =
struct
  module Config = struct
    type t = { window : Window.Window_config.t }

    let default () : t = { window = Window.Window_config.default () }
  end

  let asset_plugin = Luma__asset.Plugin.plugin
  let window_plugin = Window.plugin
  let time_plugin = Time.plugin
  let camera_plugin = Camera_plugin.plugin
  let input_plugin = Input.Keyboard.plugin
  let audio_plugin = Audio.plugin
  let sprite_plugin = Sprite_plugin.add_plugin
  let debug_plugin = Debug.add_plugin
  let transform_plugin = Transform.add_plugin
  let texture_plugin = Texture.add_plugin
  let default_config () : Config.t = { window = Window.Window_config.default () }

  (** [add_default_plugins ?config app] installs the engine’s core plugins (input, audio, window,
      camera, time, asset).

      Call order matters. Any plugins the caller has already added via [add_plugin] are preserved
      and executed before the engine defaults. Plugins added after [add_default_plugins] are
      executed later from [App.run].

      [config] – optional engine-level settings. Use [default_config ()] when unsure. *)
  let add_default_plugins ?(config : Config.t = default_config ()) app =
    let plugins = Luma__app.App.plugins app in
    let plugins =
      [
        input_plugin;
        audio_plugin;
        window_plugin ~config:config.window;
        camera_plugin;
        time_plugin;
        asset_plugin;
        sprite_plugin;
        transform_plugin;
        texture_plugin;
      ]
      @ plugins
    in
    List.fold_right (fun p a -> p a) plugins app |> Luma__app.App.clear_plugins
end
