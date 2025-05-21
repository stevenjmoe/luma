module Raylib_driver : Luma__driver.Driver.S = struct
  open Luma__math

  type camera = Raylib.Camera2D.t
  type colour = Raylib.Color.t
  type texture = Raylib.Texture2D.t
  type sound = Raylib.Sound.t

  let draw_rect rect colour =
    Raylib.draw_rectangle
      (Int.of_float (Rect.x rect))
      (Int.of_float (Rect.y rect))
      (Int.of_float (Rect.width rect))
      (Int.of_float (Rect.height rect))
      colour

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

  module Audio = struct
    let init_audio_device = Raylib.init_audio_device
    let close_audio_device = Raylib.close_audio_device
  end

  module Sound = struct
    type t = sound

    let load_sound = Raylib.load_sound
    let play_sound = Raylib.play_sound
    let stop_sound = Raylib.stop_sound
    let pause_sound = Raylib.pause_sound
    let resume_sound = Raylib.resume_sound
    let is_sound_playing = Raylib.is_sound_playing
    let set_sound_volume = Raylib.set_sound_volume
    let set_sound_pan = Raylib.set_sound_pan
    let unload_sound = Raylib.unload_sound
  end
end

include Raylib_driver
