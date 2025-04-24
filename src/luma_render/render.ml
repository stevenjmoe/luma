module type Renderer = sig
  type texture

  val draw_texture :
    texture ->
    position:Luma__math.Vec2.t ->
    size:Luma__math.Vec2.t ->
    ?flip_x:bool ->
    ?flip_y:bool ->
    ?frame_index:int ->
    ?texture_atlas:Luma__image.Texture_atlas.t option ->
    unit ->
    unit
end

module Make (D : Luma__driver.Driver.S) : Renderer with type texture = D.texture = struct
  open Luma__math
  open Luma__image

  type texture = D.Texture.t

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
      Rect.create (Vec2.create 0. 0.)
        (Vec2.create
           (D.Texture.width texture |> Float.of_int)
           (D.Texture.height texture |> Float.of_int))
    in
    let src_rect =
      match texture_atlas with
      | None -> create_rect texture
      | Some atlas -> (
          match Texture_atlas.get_frame atlas frame_index with
          | None -> create_rect texture
          | Some frame ->
              Rect.create
                (Vec2.create frame.min.x frame.min.y)
                (Vec2.create (frame.max.x -. frame.min.x) (frame.max.y -. frame.min.y)))
    in
    let x = Rect.x src_rect in
    let y = Rect.y src_rect in
    let w = Rect.width src_rect in
    let h = Rect.height src_rect in

    let src_rect =
      if flip_x then
        Rect.create (Vec2.create (x +. w) y) (Vec2.create (-.w) h)
      else if flip_y then
        Rect.create (Vec2.create x (y +. h)) (Vec2.create w (-.h))
      else
        src_rect
    in
    let dest_rec =
      Rect.create
        (Vec2.create (Vec2.x position) (Vec2.y position))
        (Vec2.create (Vec2.x size) (Vec2.y size))
    in
    D.Texture.draw_texture texture src_rect dest_rec Vec2.zero 0.0 D.Colour.white
end
