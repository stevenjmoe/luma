module type S = sig
  open Luma__app
  open Luma__ecs
  open Luma__asset
  open Luma__image
  open Luma__sprite
  open Luma__transform
  open Luma__time
  open Luma__math
  open Luma__resource
  open Luma__id
  open Luma__audio
  open Luma__type_register
  open Luma__scene
  open Luma__serialize
  open Luma__window
  open Luma__input
  open Luma__ui
  open Luma__debug
  open Luma__plugin
  open Luma__core
  module Types = Luma__types

  module App : sig
    include module type of App

    val run : t -> unit
  end

  type colour
  type texture
  type sound

  module Draw : sig
    val draw_text : string -> int -> int -> int -> colour -> unit
  end

  module Window_config : Luma__window.Window.Window_config with type colour = colour
  module Camera_config : module type of Luma__render.Render.Camera_config
  module Input : Luma__input.Input.S
  module Ui : Luma__ui.Ui.S
  module Plugin : Plugin.S with type app = App.t and type window = Window_config.t

  module Image : sig
    module Texture : Texture.S with type t = texture
    module Texture_atlas : module type of Texture_atlas
    module Texture_atlas_layout : module type of Texture_atlas_layout
  end

  module Audio : Audio.S

  module Colour : sig
    type t = colour

    val rgb : r:int -> g:int -> b:int -> t
    val rgba : r:int -> g:int -> b:int -> a:int -> t
    val white : t
    val from_string : string -> (t, Luma__core.Error.error) result
  end

  module Camera : Luma__render.Camera.S
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
  module Time_plugin : Time.PLUGIN
  module Transform : module type of Transform
  module World : module type of World
  module Math : module type of Luma__math
  module Texture_atlas : module type of Texture_atlas
  module Texture_atlas_layout : module type of Texture_atlas_layout

  module Renderer :
    Luma__render.Render.Renderer with type texture = texture and type colour = colour

  module Sprite : Sprite.S
  module Key : module type of Luma__types.Input_types.Key
  module Mouse_button : module type of Luma__types.Input_types.Mouse_button
  module State : module type of Luma__state.State
  module Scene : Scene.S
  module IO : module type of Luma__driver.Driver.IO
  module Command : module type of Command

  val screen_width : unit -> int
  val screen_height : unit -> int

  module Log : sig
    val log : ('a, Format.formatter, unit, unit) format4 -> 'a
    val error : ('a, unit) Luma__core__Log.conditional_log
    val warn : ('a, unit) Luma__core__Log.conditional_log
    val debug : ('a, unit) Luma__core__Log.conditional_log
    val info : ('a, unit) Luma__core__Log.conditional_log
  end

  module Error : module type of Luma__core.Error
end

module Make (D : Luma__driver.Driver.S) : S = struct
  open Luma__app
  open Luma__ecs
  open Luma__asset
  module Render = Luma__render
  open Luma__image
  open Luma__sprite
  open Luma__transform
  open Luma__time
  open Luma__math
  open Luma__resource
  open Luma__id
  open Luma__audio
  open Luma__type_register
  open Luma__scene
  open Luma__serialize
  open Luma__window
  open Luma__input
  open Luma__ui
  open Luma__debug
  open Luma__plugin
  open Luma__core
  open Luma__state
  module Types = Luma__types

  module Image = struct
    module Texture = Texture.Make (D)
    module Texture_atlas = Texture_atlas
    module Texture_atlas_layout = Texture_atlas_layout
  end

  module Camera_config = Luma__render.Render.Camera_config
  module R = Luma__render.Render.Make (D) (Sprite) (Image.Texture)

  type texture = Image.Texture.t

  module Draw = struct
    let draw_text = D.Draw.draw_text
  end

  (* core driver dependent modules *)
  module Window = Window.Make (D)
  module Camera = R.Camera
  module Input = Input.Make (D)
  module Ui = Ui.Make (D)
  module Time = Time
  module Time_plugin = Time.Plugin (D)
  module Audio = Audio.Make (D)
  module Debug_plugin = Debug.Make (D) (Camera)
  module Scene = Scene.Make (D)

  type colour = D.colour
  type sound = Audio.Sound.t

  module Renderer = struct
    include R

    type nonrec texture = texture
    type nonrec colour = colour
  end

  module Plugin =
    Plugin.Make (D) (Window) (Renderer) (Input) (Time_plugin) (Audio) (Image.Texture) (Scene)
      (Debug_plugin)

  module Window_config = Window.Window_config

  module App = struct
    include App

    let run app = run (module D) app
  end

  (* logging *)
  let default_log = Log.sub_log (Logs.Src.name Logs.default)

  module Log = struct
    let log = Log.app_log
    let error = default_log.error
    let warn = default_log.warn
    let debug = default_log.debug
    let info = default_log.info
  end

  let () =
    Fmt_tty.setup_std_outputs ();
    Luma__core.Log.init ()

  module Sprite = Sprite

  module Colour = struct
    type t = colour

    include D.Colour
  end

  module Archetype = Archetype
  module Asset = Asset
  module Assets = Assets
  module Asset_server = Server
  module Asset_loader = Loader
  module Component = Component
  module Id = Id
  module Query = Query
  module Resource = Resource
  module Scheduler = Scheduler
  module System = System
  module Transform = Transform
  module World = World
  module Math = Luma__math
  module Texture_atlas = Texture_atlas
  module Texture_atlas_layout = Texture_atlas_layout
  module Key = Types.Input_types.Key
  module Mouse_button = Types.Input_types.Mouse_button
  module State = State
  module IO = D.IO
  module Command = Command

  let screen_width = D.Window.screen_width
  let screen_height = D.Window.screen_height

  module Error = Error
end
