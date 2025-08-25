module type Renderer = sig
  type texture
  type colour

  val draw_rect : Luma__math.Rect.t -> colour -> unit
  val draw_rect_lines : Luma__math.Rect.t -> float -> colour -> unit

  val draw_texture :
    texture ->
    position:Luma__math.Vec2.t ->
    size:Luma__math.Vec2.t ->
    ?flip_x:bool ->
    ?flip_y:bool ->
    ?texture_atlas:Luma__image.Texture_atlas.t option ->
    unit ->
    unit

  val draw_circle : int -> int -> float -> colour -> unit
end

module Make (D : Luma__driver.Driver.S) :
  Renderer with type texture = D.texture and type colour = D.colour = struct
  open Luma__math
  open Luma__image

  type texture = D.Texture.t
  type colour = D.colour

  let draw_rect rect colour = D.draw_rect rect colour
  let draw_rect_lines rect line colour = D.draw_rect_lines rect line colour
  let draw_circle center_x center_y radius colour = D.draw_circle center_x center_y radius colour

  let draw_texture
      texture
      ~position
      ~size
      ?(flip_x = false)
      ?(flip_y = false)
      ?(texture_atlas = None)
      () =
    let create_rect texture =
      Rect.create ~pos:(Vec2.create 0. 0.)
        ~size:
          (Vec2.create
             (D.Texture.width texture |> Float.of_int)
             (D.Texture.height texture |> Float.of_int))
    in
    let src_rect =
      match texture_atlas with
      | None -> create_rect texture
      | Some atlas -> (
          match Texture_atlas.get_frame atlas with
          | None -> create_rect texture
          | Some frame -> frame)
    in
    let x = Rect.x src_rect in
    let y = Rect.y src_rect in
    let w = Rect.width src_rect in
    let h = Rect.height src_rect in

    let default = Rect.create ~pos:(Vec2.create x y) ~size:(Vec2.create w h) in
    let src_rect =
      match (flip_x, flip_y) with
      | false, false -> default
      | true, false -> Rect.create ~pos:(Vec2.create x y) ~size:(Vec2.create (-.w) h)
      | false, true -> Rect.create ~pos:(Vec2.create x y) ~size:(Vec2.create w (-.h))
      | true, true ->
          Rect.create ~pos:(Vec2.create (x +. w) (y +. h)) ~size:(Vec2.create (-.w) (-.h))
    in
    let dest_rec = Rect.create ~pos:position ~size in
    D.Texture.draw_texture texture src_rect dest_rec Vec2.zero 0.0 D.Colour.white
end

module Camera = struct
  include Camera_component
  module Plugin = Camera_plugin
end
