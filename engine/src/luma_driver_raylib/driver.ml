module Raylib_driver : Luma__driver.Driver.S = struct
  open Luma__math
  open Luma__core

  type camera = Raylib.Camera2D.t
  type colour = Raylib.Color.t
  type texture = Raylib.Texture2D.t
  type sound = Raylib.Sound.t
  type music = Raylib.Music.t

  let get_frame_time = Raylib.get_frame_time

  module Draw = struct
    let draw_text text x y size colour = Raylib.draw_text text x y size colour

    let draw_circle center_x center_y radius colour =
      Raylib.draw_circle center_x center_y radius colour

    let draw_rect rect colour =
      Raylib.draw_rectangle
        (Int.of_float (Rect.x rect))
        (Int.of_float (Rect.y rect))
        (Int.of_float (Rect.width rect))
        (Int.of_float (Rect.height rect))
        colour

    let draw_rect_lines rect line colour =
      Raylib.draw_rectangle_lines_ex
        (Raylib.Rectangle.create (Rect.x rect) (Rect.y rect) (Rect.width rect) (Rect.height rect))
        line colour
  end

  module IO = struct
    type path = string

    let run_io_loop () = Luv.Loop.run ~mode:`NOWAIT ~loop:(Luv.Loop.default ()) () |> ignore

    let read_file path ~k =
      let flags = [ `RDONLY ] in
      Luv.File.open_ path flags (function
        | Error e ->
            let msg = Luv.Error.strerror e in
            k (Error (Error.io_read path msg))
        | Ok file ->
            let buffer = Luv.Buffer.create 65536 in
            let acc = Stdlib.Buffer.create 65536 in

            let rec loop offset =
              Luv.File.read file [ buffer ] ~file_offset:offset (function
                | Error e ->
                    Luv.File.close file (fun _ ->
                        let msg = Luv.Error.strerror e in
                        k (Error (Error.io_read path msg)))
                | Ok n ->
                    let n = Unsigned.Size_t.to_int n in
                    if n = 0 then
                      Luv.File.close file (fun _ ->
                          let s = Stdlib.Buffer.contents acc in
                          k (Ok (Bytes.unsafe_of_string s)))
                    else
                      let chunk = Bytes.sub_string (Luv.Buffer.to_bytes buffer) 0 n in
                      Stdlib.Buffer.add_string acc chunk;
                      loop Int64.(add offset (of_int n)))
            in
            loop 0L)

    let read_file_blocking path = Core.In_channel.read_all path |> Bytes.of_string

    (* TODO: Error *)
    let write_file (path : string) (bytes : bytes) : unit =
      let flags = [ `CREAT; `TRUNC; `WRONLY ] in
      Luv.File.open_ path flags (function
        | Error _ -> ()
        | Ok file ->
            let s = Bytes.unsafe_to_string bytes in
            let buf = Luv.Buffer.from_string s in
            Luv.File.write file [ buf ] ~file_offset:0L (function
              | Error _ -> Luv.File.close file (fun _ -> ())
              | Ok _ -> Luv.File.close file (fun _ -> ())))
  end

  module Window = Window
  module Camera = Camera
  module Input = Input

  module Colour = struct
    let rgb ~r ~g ~b = Raylib.Color.create r g b 255
    let rgba ~r ~g ~b ~a = Raylib.Color.create r g b a
    let white = Raylib.Color.white
  end

  module Image = struct
    type t = Raylib.Image.t

    let load_image = Raylib.load_image
    let load_image_from_memory = Raylib.load_image_from_memory
  end

  module Texture = struct
    type t = texture

    include Texture
  end

  module Text = struct end

  module Audio = struct
    let init_audio_device = Raylib.init_audio_device
    let close_audio_device = Raylib.close_audio_device

    module Sound = struct
      type t = sound

      let load_sound = Raylib.load_sound
      let play_sound sound = if Raylib.is_sound_ready sound then Raylib.play_sound sound
      let stop_sound = Raylib.stop_sound
      let pause_sound = Raylib.pause_sound
      let resume_sound = Raylib.resume_sound
      let is_sound_playing = Raylib.is_sound_playing
      let set_sound_volume = Raylib.set_sound_volume
      let set_sound_pan = Raylib.set_sound_pan
      let unload_sound = Raylib.unload_sound
    end

    module Music = struct
      type t = music

      let load_music_stream = Raylib.load_music_stream
      let is_music_ready = Raylib.is_music_ready
      let unload_music_stream = Raylib.unload_music_stream
      let play_music_stream = Raylib.play_music_stream
      let is_music_stream_playing = Raylib.is_music_stream_playing
      let update_music_stream = Raylib.update_music_stream
      let stop_music_stream = Raylib.stop_music_stream
      let pause_music_stream = Raylib.pause_music_stream
      let resume_music_stream = Raylib.resume_music_stream
      let seek_music_stream = Raylib.seek_music_stream
      let set_music_volume = Raylib.set_music_volume
      let set_music_pan = Raylib.set_music_pan
      let get_music_time_length = Raylib.get_music_time_length
      let get_music_time_played = Raylib.get_music_time_played
    end
  end

  module UI = struct
    open Raylib

    type win = {
      x : float;
      y : float;
      w : float;
      h : float;
      mutable cursor_y : float;
      title : string;
    }

    let win_stack : win list ref = ref []
    let window_open : (string, bool) Hashtbl.t = Hashtbl.create 16
    let title_bar_h = 28.0
    let pad_x = 8.0
    let line_h = 18.0
    let line_gap = 2.0

    let begin_window ~title ?pos ?size () =
      let x = Option.map (fun (v : Vec2.t) -> v.x) pos |> Option.value ~default:20. in
      let y = Option.map (fun (v : Vec2.t) -> v.y) pos |> Option.value ~default:20. in
      let w = Option.map (fun (v : Vec2.t) -> v.x) size |> Option.value ~default:360. in
      let h = Option.map (fun (v : Vec2.t) -> v.y) size |> Option.value ~default:220. in
      let rect = Rectangle.create x y w h in

      let active = Option.value (Hashtbl.find_opt window_open title) ~default:true in
      if not active then false
      else
        let closed = Raygui.window_box rect title in
        if closed then Hashtbl.replace window_open title false;

        let wctx = { x; y; w; h; cursor_y = y +. title_bar_h; title } in
        win_stack := wctx :: !win_stack;
        true

    let end_window () = match !win_stack with _ :: rest -> win_stack := rest | [] -> ()

    let text s =
      match !win_stack with
      | [] -> ()
      | w ->
          let w = List.hd w in
          let label_bounds =
            Rectangle.create (w.x +. pad_x) w.cursor_y (w.w -. (2.0 *. pad_x)) line_h
          in
          Raygui.label label_bounds s;
          w.cursor_y <- w.cursor_y +. line_h +. line_gap
  end

  module Debug_draw = struct
    type space =
      [ `World
      | `Screen
      ]

    let line space ~p0 ~p1 ~colour = ()
  end
end

include Raylib_driver
