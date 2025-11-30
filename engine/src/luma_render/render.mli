open Luma__app
open Luma__math
open Luma__image
open Luma__sprite

module Camera_config : sig
  type t

  val default : unit -> t
  val create : default_camera:bool -> t
  val default_camera : t -> bool
end

module type Renderer = sig
  type texture
  type colour

  module Shape : Shape.S with type colour = colour

  val draw_rect : Rect.t -> colour -> unit
  val draw_rect_lines : Rect.t -> float -> colour -> unit

  val draw_texture :
    texture ->
    position:Vec2.t ->
    size:Luma__math.Vec2.t ->
    ?flip_x:bool ->
    ?flip_y:bool ->
    ?texture_atlas:Texture_atlas.t option ->
    ?src:Luma__math.Rect.t option ->
    ?opacity:float ->
    ?rotation:float ->
    ?origin:Vec2.t ->
    unit ->
    unit

  val draw_circle : center_x:float -> center_y:float -> radius:float -> colour -> unit
  val draw_circle_lines : center_x:float -> center_y:float -> radius:float -> colour -> unit
  val plugin : ?camera_config:Camera_config.t -> App.t -> App.t

  module Queue : sig
    type sprite_cmd

    type cmd =
      | Rect of Rect.t * colour
      | Rect_lines of Rect.t * float * colour
      | ScreenRect of Rect.t * colour
      | Circle of Luma__math.Primitives.Circle.t * colour
      | Circle_lines of Luma__math.Primitives.Circle.t * colour
      | Sprite of sprite_cmd

    type meta
    type item
    type t = item list ref

    val make : unit -> t
    val clear : t -> unit
    val push : t -> item -> unit
    val iter_sorted : t -> camera_layers:int64 -> f:(item -> unit) -> unit

    module R : Luma__resource.Resource.S with type t = t
  end

  module Draw : sig
    val rect : rect:Rect.t -> colour:colour -> Queue.item list ref -> unit
    val rect_screen : rect:Rect.t -> colour:colour -> Queue.item list ref -> unit
  end

  val push_rect : z:int -> rect:Rect.t -> ?layers:int64 -> colour -> Queue.item list ref -> unit

  val push_rect_lines :
    z:int ->
    rect:Rect.t ->
    ?layers:int64 ->
    ?line_thickness:float ->
    colour ->
    Queue.item list ref ->
    unit

  val push_circle :
    z:int -> circle:Primitives.Circle.t -> ?layers:int64 -> colour -> Queue.item list ref -> unit

  val push_circle_lines :
    z:int -> circle:Primitives.Circle.t -> ?layers:int64 -> colour -> Queue.item list ref -> unit

  val push_texture :
    z:int ->
    tex:texture ->
    position:Vec2.t ->
    size:Vec2.t ->
    ?layers:int64 ->
    ?texture_atlas:Texture_atlas.t ->
    ?src:Luma__math.Rect.t ->
    ?flip_x:bool ->
    ?flip_y:bool ->
    ?opacity:float ->
    ?rotation:float ->
    ?origin:Vec2.t ->
    Queue.item list ref ->
    unit ->
    unit

  val push_rect_screen :
    z:int -> ?layers:int64 -> rect:Rect.t -> colour -> Queue.item list ref -> unit

  module Camera : Camera.S
end

module Make : functor
  (D : Luma__driver.Driver.S)
  (Sprite : Sprite.S)
  (Texture : Texture.S with type t = D.texture)
  -> Renderer with type texture = D.texture and type colour = D.colour
