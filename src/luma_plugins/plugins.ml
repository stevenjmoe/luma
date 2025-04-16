let add_default_plugins app =
  app
  |> Luma__app.App.add_plugin Luma__time.Time.plugin
  |> Luma__app.App.add_plugin Luma__asset.Plugin.plugin
  |> Luma__app.App.add_plugin
       (Luma__render.Camera.plugin (module Luma__driver.Driver.Raylib_driver))
