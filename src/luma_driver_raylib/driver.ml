module Raylib_driver : Luma__driver.Driver.S = struct
  open Luma__math

  type camera = Raylib.Camera2D.t
  type colour = Raylib.Color.t
  type texture = Raylib.Texture2D.t
  type sound = Raylib.Sound.t
  type music = Raylib.Music.t

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

  let draw_circle center_x center_y radius colour =
    Raylib.draw_circle center_x center_y radius colour

  let get_frame_time = Raylib.get_frame_time

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
end

include Raylib_driver
