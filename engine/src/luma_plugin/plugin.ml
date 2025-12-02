open Luma__app.App
open Luma__driver
open Luma__type_register
open Luma__transform
open Luma__image
open Luma__scene
open Luma__render
open Luma__window
open Luma__input
open Luma__time
open Luma__audio
open Luma__sprite
open Luma__debug

module type Config = sig
  type t
  type window

  val default : unit -> t
  val window : t -> window
  val camera : t -> Render.Camera_config.t
  val create : ?window:window -> ?camera:Render.Camera_config.t -> unit -> t
end

module type S = sig
  type window
  type config
  type app

  module Config : Config with type window = window and type t = config

  val asset_plugin : app -> app
  val window_plugin : ?config:window -> app -> app
  val time_plugin : app -> app
  val input_plugin : app -> app
  val audio_plugin : app -> app
  val sprite_plugin : app -> app
  val debug_plugin : app -> app
  val transform_plugin : app -> app
  val texture_plugin : app -> app
  val scene_plugin : app -> app
  val render_plugin : ?camera_config:Render.Camera_config.t -> app -> app
  val default_config : unit -> config
  val add_default_plugins : ?config:config -> app -> app
end

module Make
    (D : Driver.S)
    (Window : Window.S)
    (Renderer : Render.Renderer)
    (Input : Input.S)
    (Time : Time.S)
    (Audio : Audio.S)
    (Sprite_plugin : Sprite.Sprite_plugin)
    (Texture : Texture.S)
    (Scene : Scene.S)
    (Debug : Debug.S) : S with type window = Window.Window_config.t and type app = Luma__app.App.t =
struct
  module Config : Config with type window = Window.Window_config.t = struct
    type window = Window.Window_config.t

    type t = {
      window : window;
      camera : Render.Camera_config.t;
    }

    let default () : t =
      { window = Window.Window_config.default (); camera = Render.Camera_config.default () }

    let create
        ?(window = Window.Window_config.default ())
        ?(camera = Render.Camera_config.default ())
        () =
      { window; camera }

    let window c = c.window
    let camera c = c.camera
  end

  type config = Config.t
  type window = Config.window
  type app = Luma__app.App.t

  let asset_plugin = Luma__asset.Plugin.plugin
  let window_plugin = Window.plugin
  let time_plugin = Time.plugin
  let input_plugin = Input.Keyboard.plugin
  let audio_plugin = Audio.plugin
  let sprite_plugin = Sprite_plugin.add_plugin
  let debug_plugin = Debug.add_plugin
  let transform_plugin = Transform.add_plugin
  let texture_plugin = Texture.add_plugin
  let scene_plugin = Scene.add_plugin
  let render_plugin = Renderer.plugin
  let default_config () : config = Config.default ()

  let add_default_plugins ?(config : config = default_config ()) app =
    let plugins = Luma__app.App.plugins app in
    let plugins =
      [
        input_plugin;
        audio_plugin;
        window_plugin ~config:(Config.window config);
        time_plugin;
        asset_plugin;
        sprite_plugin;
        transform_plugin;
        texture_plugin;
        scene_plugin;
        render_plugin ~camera_config:(Config.camera config);
      ]
      @ plugins
    in
    List.fold_right (fun p a -> p a) plugins app |> Luma__app.App.clear_plugins
end
