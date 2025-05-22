module type S = sig
  type t

  module A : Luma__asset.Asset.S with type t = t

  val play_sound : t -> unit
  val plugin : Luma__app.App.t -> Luma__app.App.t
end

module Make (D : Luma__driver.Driver.S) : S with type t = D.Audio.Sound.t = struct
  type t = D.Audio.Sound.t

  module A = Luma__asset.Asset.Make (struct
    type inner = t
  end)

  let play_sound = D.Audio.Sound.play_sound

  let () =
    Luma__asset.Server.register_loader_hook (fun server ->
        Luma__asset.Server.register_loader server
          {
            Luma__asset.Loader.exts = [ ".wav"; ".ogg"; ".mp3"; ".qoa"; ".xm"; ".mod"; ".flac" ];
            load =
              (fun path ->
                let sound = D.Audio.Sound.load_sound path in
                Ok (Loaded ((module A), sound)));
            type_id = A.type_id;
          })

  (* TODO: unload the asset without a handle? *)
  let cleanup () =
    Luma__ecs.System.make_with_resources ~components:End
      ~resources:Luma__ecs.Query.Resource.(Resource (module Luma__asset.Assets.R) & End)
      "sound.cleanup"
      (fun world _ (assets, _) ->
        let sounds = Luma__asset.Assets.get_all (module A) assets in
        sounds |> List.iter (fun s -> D.Audio.Sound.unload_sound s);
        world)

  let plugin app = app |> Luma__app.App.add_system (Cleanup (WithResources (cleanup ())))
end
