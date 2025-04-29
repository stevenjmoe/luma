module type S = sig
  open Luma__app
  open Luma__ecs
  open Luma__asset
  open Luma__render
  open Luma__image
  open Luma__sprite
  open Luma__transform
  open Luma__time
  open Luma__math
  open Luma__resource
  open Luma__id

  module App : sig
    include module type of App

    val run : t -> unit
  end

  type colour
  type texture

  module Window_config : Luma__window.Window.Window_config with type colour = colour
  module Input : Luma__input.Input.S

  module Plugin : sig
    module Config : sig
      type t = { window : Window_config.t }

      val default : unit -> t
    end

    val add_default_plugins : ?config:Config.t -> App.t -> App.t
    val camera_plugin : App.t -> App.t
    val window_plugin : ?config:Window_config.t -> App.t -> App.t
    val asset_plugin : App.t -> App.t
    val time_plugin : App.t -> App.t
    val input_plugin : App.t -> App.t
  end

  module Image : sig
    module Texture : Texture.S with type t = texture
    module Texture_atlas : module type of Texture_atlas
    module Texture_atlas_layout : module type of Texture_atlas_layout
  end

  module Colour : sig
    type t = colour

    val rgb : r:int -> g:int -> b:int -> t
    val rgba : r:int -> g:int -> b:int -> a:int -> t
    val white : t
  end

  module Camera : Camera_component.S
  module Archetype : module type of Archetype
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
  module Transform : module type of Transform
  module World : module type of World
  module Math : module type of Luma__math
  module Texture_atlas : module type of Texture_atlas
  module Texture_atlas_layout : module type of Texture_atlas_layout
  module Sprite : Sprite.S with type texture = texture
  module Renderer : Render.Renderer with type texture = texture and type colour = colour
  module Key : module type of Luma__types.Key

  val screen_width : unit -> int
  val screen_height : unit -> int
  val log : ('a, Format.formatter, unit, unit) format4 -> 'a
end

module Make (D : Luma__driver.Driver.S) : S = struct
  (* core driver dependent modules *)
  module Window = Luma__window.Window.Make (D)
  module Camera_component = Luma__render.Camera_component.Make (D)
  module Camera_plugin = Luma__render.Camera_plugin.Make (D) (Camera_component)
  module Input = Luma__input.Input.Make (D)
  module Plugin = Luma__plugin.Plugin.Make (D) (Window) (Camera_plugin) (Input)
  module Window_config = Window.Window_config

  module App = struct
    include Luma__app.App

    let run app = run (module D) app
  end

  module Image = struct
    module Texture = Luma__image.Texture.Make (D)
    module Texture_atlas = Luma__image.Texture_atlas
    module Texture_atlas_layout = Luma__image.Texture_atlas_layout
  end

  type colour = D.colour
  type texture = Image.Texture.t

  module S = Luma__sprite.Sprite.Make (Image.Texture)
  module R = Luma__render.Render.Make (D)

  module Renderer = struct
    include R

    type nonrec texture = texture
    type nonrec colour = colour
  end

  let () =
    Fmt_tty.setup_std_outputs ();
    Luma__core.Log.init ()

  module Sprite = struct
    include S

    type nonrec texture = texture
  end

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
  module Texture_atlas = Luma__image.Texture_atlas
  module Texture_atlas_layout = Luma__image.Texture_atlas_layout
  module Key = Luma__types.Key

  let screen_width = D.Window.screen_width
  let screen_height = D.Window.screen_height
  let log = Luma__core.Log.app_log
end
