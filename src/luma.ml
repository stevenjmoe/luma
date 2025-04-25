module Make (D : Luma__driver.Driver.S) = struct
  (* core driver dependent modules *)
  module Window = Luma__window.Window.Make (D)
  module Camera_component = Luma__render.Camera_component.Make (D)
  module Camera_plugin = Luma__render.Camera_plugin.Make (D) (Camera_component)
  module Plugins = Luma__plugins.Plugins.Make (D) (Window) (Camera_plugin)
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

  let screen_width = D.Window.screen_width
  let screen_height = D.Window.screen_height
end
