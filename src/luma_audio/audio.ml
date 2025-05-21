module type S = sig
  val plugin : Luma__app__App.t -> Luma__app__App.t

  module Sound : Sound.S
end

module Make (D : Luma__driver.Driver.S) : S = struct
  module Sound = Sound.Make (D)

  let init () =
    Luma__ecs.System.make ~components:End "audio.init" (fun world _ ->
        D.Audio.init_audio_device ();
        world)

  let cleanup () =
    Luma__ecs.System.make ~components:End "audio.cleanup" (fun world _ ->
        D.Audio.close_audio_device ();
        world)

  let plugin app =
    app
    |> Luma__app.App.add_system (PreStartup (WithoutResources (init ())))
    |> Luma__app.App.add_system (Cleanup (WithoutResources (cleanup ())))
    |> Sound.plugin
end
