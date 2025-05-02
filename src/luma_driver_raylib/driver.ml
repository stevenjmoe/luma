module Raylib_driver : Luma__driver.Driver.S = struct
  open Luma__math

  type camera = Raylib.Camera2D.t
  type colour = Raylib.Color.t
  type texture = Raylib.Texture2D.t

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
end

include Raylib_driver
