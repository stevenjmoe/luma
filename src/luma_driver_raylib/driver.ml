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
      colour;
    ()

  module Window = struct
    let init ~width ~height ~title =
      Raylib.init_window width height title;
      Raylib.set_target_fps 60

    let shutdown () = Raylib.close_window ()
    let should_close () = Raylib.window_should_close ()
    let get_frame_time () = Raylib.get_frame_time ()
    let begin_frame () = Raylib.begin_drawing ()
    let end_frame () = Raylib.end_drawing ()
    let begin_2d = Raylib.begin_mode_2d
    let end_2d () = Raylib.end_mode_2d ()
    let clear = Raylib.clear_background
    let screen_width = Raylib.get_screen_width
    let screen_height = Raylib.get_screen_height
  end

  module Camera = struct
    let make ~(position : Vec2.t) ~(target : Vec2.t) ~rotation ~zoom =
      let px, py = (Vec2.x position, Vec2.y position) and tx, ty = (Vec2.x target, Vec2.y target) in
      Raylib.Camera2D.create (Raylib.Vector2.create px py) (Raylib.Vector2.create tx ty) rotation
        zoom

    let default () =
      let position = Raylib.Vector2.create 140. 0. in
      let offset =
        Raylib.Vector2.create
          (Float.of_int (Raylib.get_screen_width ()) /. 2.)
          (Float.of_int (Raylib.get_screen_height ()) /. 2.)
      in
      let target = Raylib.Vector2.create (Raylib.Vector2.x position) (Raylib.Vector2.y position) in
      Raylib.Camera2D.create offset target 0. 1.

    let set_target camera target =
      let x, y = target in
      Raylib.Camera2D.set_target camera (Raylib.Vector2.create x y);
      ()
  end

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
    open Raylib
    open Luma__math

    type t = texture

    let width = Raylib.Texture2D.width
    let height = Raylib.Texture2D.height

    let draw_texture texture source dest origin rotation tint =
      let source =
        Rectangle.create (Rect.x source) (Rect.y source) (Rect.width source) (Rect.height source)
      in
      let dest =
        Rectangle.create (Rect.x dest) (Rect.y dest) (Rect.width dest) (Rect.height dest)
      in
      let origin = Vector2.create (Vec2.x origin) (Vec2.y origin) in
      Raylib.draw_texture_pro texture source dest origin rotation tint

    let load_texture_from_image = Raylib.load_texture_from_image
  end
end

include Raylib_driver
