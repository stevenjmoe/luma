module type S = sig
  type music
  type t

  module A : Luma__asset.Asset.S with type t = t

  val play : ?volume:float -> ?loop:bool -> t -> unit
  val pause : t -> unit
  val resume : t -> unit
  val stop : t -> unit
  val set_volume : t -> float -> unit
  val set_pan : t -> float -> unit
  val is_playing : t -> bool
  val progress : t -> float option
  val plugin : Luma__app.App.t -> Luma__app.App.t
end

module Make (D : Luma__driver.Driver.S) : S with type music = D.Audio.Music.t = struct
  type music = D.Audio.Music.t

  type t = {
    stream : music;
    mutable state : [ `Stopped | `Playing | `Paused ];
    mutable volume : float;
    mutable pan : float;
    mutable loop : bool;
  }

  module A = Luma__asset.Asset.Make (struct
    type inner = t
  end)

  let set_state t state = t.state <- state
  let get_state t = t.state

  let set_volume t vol =
    t.volume <- vol;
    D.Audio.Music.set_music_volume t.stream vol

  let set_pan t pan =
    t.pan <- pan;
    D.Audio.Music.set_music_pan t.stream pan

  let is_playing t = match t.state with `Playing -> true | _ -> false

  let play ?(volume = 1.) ?(loop = false) t =
    set_volume t volume;
    t.loop <- loop;
    D.Audio.Music.play_music_stream t.stream;
    set_state t `Playing

  let pause t =
    if t.state = `Playing then (
      D.Audio.Music.pause_music_stream t.stream;
      set_state t `Paused)

  let resume t =
    if t.state = `Paused then (
      set_state t `Playing;
      D.Audio.Music.resume_music_stream t.stream)

  let stop t =
    if t.state <> `Stopped then (
      D.Audio.Music.stop_music_stream t.stream;
      t.state <- `Stopped)

  let progress t =
    if t.state = `Stopped then None
    else
      Some
        (D.Audio.Music.get_music_time_played t.stream
        /. D.Audio.Music.get_music_time_length t.stream)

  (* TODO: unload the asset without a handle? *)
  let cleanup () =
    Luma__ecs.System.make_with_resources ~components:End
      ~resources:Luma__ecs.Query.Resource.(Resource (module Luma__asset.Assets.R) & End)
      "music.cleanup"
      (fun world _ _ (assets, _) ->
        let music = Luma__asset.Assets.get_all (module A) assets in
        music |> List.iter (fun s -> D.Audio.Music.unload_music_stream s.stream);
        world)

  let update_music_stream () =
    Luma__ecs.System.make_with_resources ~components:End
      ~resources:Luma__ecs.Query.Resource.(Resource (module Luma__asset.Assets.R) & End)
      "music.stream"
      (fun world _ _ (assets, _) ->
        let update_music m =
          D.Audio.Music.update_music_stream m.stream;
          if m.state = `Playing && not m.loop then
            let time_played = D.Audio.Music.get_music_time_played m.stream in

            (* small buffer for floating point precision *)
            let length = D.Audio.Music.get_music_time_length m.stream -. 0.1 in

            if length > 0.0 && time_played >= length then stop m
        in
        Luma__asset.Assets.get_all (module A) assets |> List.iter update_music;
        world)

  let plugin app =
    app |> Luma__app.App.on Update (update_music_stream ()) |> Luma__app.App.on Cleanup (cleanup ())

  (*let () =
    Luma__asset.Server.register_loader_hook (fun server ->
        Luma__asset.Server.register_loader server
          {
            Luma__asset.Loader.exts = [ ".wav"; ".ogg"; ".mp3"; ".qoa"; ".xm"; ".mod"; ".flac" ];
            load =
              (fun path ->
                let stream = D.Audio.Music.load_music_stream path in
                Ok ());
            (*Ok
                  (Loaded
                     ((module A), { stream; state = `Stopped; volume = 1.; pan = 1.0; loop = false })));*)
            type_id = A.type_id;
          })*)
end
