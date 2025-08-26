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

module Make : functor (D : Luma__driver.Driver.S) ->
  Renderer with type texture = D.texture and type colour = D.colour

module Camera : module type of Camera
