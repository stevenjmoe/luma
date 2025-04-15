let add_default_plugins app =
  app
  |> Luma__ecs.App.add_plugin Luma__time.Time.plugin
  |> Luma__ecs.App.add_plugin Luma__asset.Plugin.plugin
  |> Luma__ecs.App.add_plugin (Luma__render.Camera.plugin (module Luma__ecs.App.Raylib_driver))
