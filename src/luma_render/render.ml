module type Renderer = sig
  type texture = Luma__texture.Texture.t

  val load_texture : string -> texture

  val draw_texture :
    texture ->
    position:Luma__math.Vec2.t ->
    size:Luma__math.Vec2.t ->
    ?flip_x:bool ->
    ?flip_y:bool ->
    ?frame_index:int ->
    ?texture_atlas:Luma__image.Image.Texture_atlas.t option ->
    unit ->
    unit
end

module RaylibRenderer : Renderer = struct
  open Luma__math
  open Luma__texture

  type texture = Texture.t

  let load_texture path = Luma__texture.Texture.load path

  let draw_texture
      texture
      ~position
      ~size
      ?(flip_x = false)
      ?(flip_y = false)
      ?(frame_index = 0)
      ?(texture_atlas = None)
      () =
    let create_rect texture =
      Raylib.Rectangle.create 0. 0.
        (Raylib.Texture2D.width texture |> Float.of_int)
        (Raylib.Texture2D.height texture |> Float.of_int)
    in
    let src_rect =
      match texture_atlas with
      | None -> create_rect texture
      | Some atlas -> (
          match Luma__image.Image.Texture_atlas.get_frame atlas frame_index with
          | None -> create_rect texture
          | Some frame ->
              Raylib.Rectangle.create (Float.of_int frame.min.x) (Float.of_int frame.min.y)
                (Float.of_int (frame.max.x - frame.min.x))
                (Float.of_int (frame.max.y - frame.min.y)))
    in
    let x = Raylib.Rectangle.x src_rect in
    let y = Raylib.Rectangle.y src_rect in
    let width = Raylib.Rectangle.width src_rect in
    let height = Raylib.Rectangle.height src_rect in

    let src_rect =
      if flip_x then
        Raylib.Rectangle.create (x +. width) y (-.width) height
      else if flip_y then
        Raylib.Rectangle.create x (y +. height) width (-.height)
      else
        src_rect
    in
    let dest_rec = Vec2.(Raylib.Rectangle.create position.x position.y size.x size.y) in
    Raylib.draw_texture_pro texture src_rect dest_rec (Raylib.Vector2.zero ()) 0.0
      Raylib.Color.white
end

module Renderer = RaylibRenderer
