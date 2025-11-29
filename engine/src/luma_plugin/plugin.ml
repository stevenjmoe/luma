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
    (Debug : Debug.S) =
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
  let default_config () : Config.t = Config.default ()

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
