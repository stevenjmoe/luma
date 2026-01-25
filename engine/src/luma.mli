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

  module Render : sig
    module Renderer :
      Luma__render.Render.Renderer with type texture = texture and type colour = colour

    (* driver independant modules *)
    module Types : sig
      module View : module type of Luma__render.View
    end
  end

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
    (** Main entry point: sets up the driver and runs the app. *)
  end

  module Plugin : Luma__plugin.Plugin.S with type app = App.t and type window = Window_config.t
  module Error : module type of Luma__core.Error
end

module Make : functor (_ : Luma__driver.Driver.S) -> S
