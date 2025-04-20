open Luma__app.App

module Make (D : Luma__driver.Driver.S) = struct
  module C = Luma__render.Camera_component.Make (D)
  module P = Luma__render.Camera_plugin.Make (D) (C)

  let add_default_plugins (module D : Luma__driver.Driver.S) app =
    app
    |> add_plugin Luma__time.Time.plugin
    |> add_plugin Luma__asset.Plugin.plugin
    |> add_plugin P.plugin
end
