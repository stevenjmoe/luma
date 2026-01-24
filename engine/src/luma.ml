module type S = sig
  module Driver : Luma__driver.Driver.S

  type colour
  type texture
  type sound

  module Draw : sig
    val draw_text : string -> int -> int -> int -> colour -> unit
  end

  module Ecs : sig
    module System : module type of Luma__ecs.System
    module Scheduler : module type of Luma__ecs.Scheduler
    module Command : module type of Luma__ecs.Command
    module World : module type of Luma__ecs.World
    module Component : module type of Luma__ecs.Component
    module Query : module type of Luma__ecs.Query
  end

  module Window_config : Luma__window.Window.Window_config with type colour = colour
  module Camera : module type of Luma__camera.Camera

  module Renderer :
    Luma__render.Render.Renderer with type texture = texture and type colour = colour

  module Image : sig
    module Texture : Luma__image.Texture.S with type t = texture
    module Texture_atlas : module type of Luma__image.Texture_atlas
    module Texture_atlas_layout : module type of Luma__image.Texture_atlas_layout
  end

  module Sprite : Luma__sprite.Sprite.S
  module Input : Luma__input.Input.S
  module Ui : Luma__ui.Ui.S
  module Audio : Luma__audio.Audio.S
  module Time : module type of Luma__time.Time
  module Time_plugin : Luma__time.Time.PLUGIN
  module Scene : Luma__scene.Scene.S
  module Resource : module type of Luma__resource.Resource
  module Transform : module type of Luma__transform.Transform
  module Id : module type of Luma__id.Id
  module State : module type of Luma__state.State
  module Asset : module type of Luma__asset.Asset
  module Assets : module type of Luma__asset.Assets
  module Asset_loader : module type of Luma__asset.Loader
  module Asset_server : module type of Luma__asset.Server
  module Math : module type of Luma__math
  module Serialize : module type of Luma__serialize.Serialize

  val screen_width : unit -> int
  val screen_height : unit -> int

  module Colour : sig
    type t = colour

    val rgb : r:int -> g:int -> b:int -> t
    val rgba : r:int -> g:int -> b:int -> a:int -> t
    val white : t
    val from_string : string -> (t, Luma__core.Error.error) result
  end

  module Log : sig
    val log : ('a, Format.formatter, unit, unit) format4 -> 'a
    val error : ('a, unit) Luma__core__Log.conditional_log
    val warn : ('a, unit) Luma__core__Log.conditional_log
    val debug : ('a, unit) Luma__core__Log.conditional_log
    val info : ('a, unit) Luma__core__Log.conditional_log
  end

  module App : sig
    include module type of Luma__app.App

    val run : t -> unit
  end

  module Plugin : Luma__plugin.Plugin.S with type app = App.t and type window = Window_config.t
  module Error : module type of Luma__core.Error
end

module Make (D : Luma__driver.Driver.S) : S = struct
  module Image = struct
    module Texture = Luma__image.Texture.Make (D)
    module Texture_atlas = Luma__image.Texture_atlas
    module Texture_atlas_layout = Luma__image.Texture_atlas_layout
  end

  module Driver = D

  module Ecs = struct
    module World = Luma__ecs.World
    module Component = Luma__ecs.Component
    module Query = Luma__ecs.Query
    module System = Luma__ecs.System
    module Scheduler = Luma__ecs.Scheduler
    module Command = Luma__ecs.Command
  end

  type texture = Image.Texture.t
  type colour = D.colour

  module Draw = struct
    let draw_text = D.Draw.draw_text
  end

  module Window = Luma__window.Window.Make (D)
  module Window_config = Window.Window_config
  module R = Luma__render.Render.Make (D) (Image.Texture)
  module Camera = Luma__camera.Camera

  module Renderer = struct
    include R

    type nonrec texture = texture
    type nonrec colour = colour
  end

  module Input = Luma__input.Input.Make (D)
  module Ui = Luma__ui.Ui.Make (D)
  module Time = Luma__time.Time
  module Time_plugin = Time.Plugin (D)
  module Audio = Luma__audio.Audio.Make (D)

  type sound = Audio.Sound.t

  module Scene = Luma__scene.Scene.Make (D)

  module App = struct
    include Luma__app.App

    let run app = run (module D) app
  end

  module Sprite = Luma__sprite.Sprite
  module Resource = Luma__resource.Resource
  module Transform = Luma__transform.Transform
  module Id = Luma__id.Id
  module State = Luma__state.State
  module Asset = Luma__asset.Asset
  module Assets = Luma__asset.Assets
  module Asset_loader = Luma__asset.Loader
  module Asset_server = Luma__asset.Server
  module Math = Luma__math
  module Debug = Luma__debug.Debug.Make (D) (R)

  module Plugin =
    Luma__plugin.Plugin.Make (Driver) (Window) (Renderer) (Input) (Time_plugin) (Audio)
      (Image.Texture)
      (Scene)
      (Debug)

  module Serialize = Luma__serialize.Serialize

  let screen_width = D.Window.screen_width
  let screen_height = D.Window.screen_height

  module Colour = struct
    type t = colour

    include D.Colour
  end

  let () =
    Fmt_tty.setup_std_outputs ();
    Luma__core.Log.init ()

  module Log = struct
    let default_log = Luma__core.Log.sub_log (Logs.Src.name Logs.default)
    let log = Luma__core.Log.app_log
    let error = default_log.error
    let warn = default_log.warn
    let debug = default_log.debug
    let info = default_log.info
  end

  module Error = Luma__core.Error
end
