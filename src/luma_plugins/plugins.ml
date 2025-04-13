let add_default_plugins app =
  app |> Luma__render.Camera.plugin |> Luma__time.Time.plugin |> Luma__asset.Plugin.plugin
