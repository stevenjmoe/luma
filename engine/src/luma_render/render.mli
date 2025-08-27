open Luma__app
open Luma__math
open Luma__image

module Camera_config : sig
  type t

  val default : unit -> t
  val make : default_camera:bool -> t
  val default_camera : t -> bool
end

module type Renderer = sig
  type texture
  type colour

  val draw_rect : Rect.t -> colour -> unit
  val draw_rect_lines : Rect.t -> float -> colour -> unit

  val draw_texture :
    texture ->
    position:Vec2.t ->
    size:Luma__math.Vec2.t ->
    ?flip_x:bool ->
    ?flip_y:bool ->
    ?texture_atlas:Texture_atlas.t option ->
    unit ->
    unit

  val draw_circle : int -> int -> float -> colour -> unit
  val plugin : ?camera_config:Camera_config.t -> App.t -> App.t

  module Queue : sig
    type sprite

    type cmd =
      | Rect of Rect.t * colour
      | Rect_lines of Rect.t * float * colour
      | ScreenRect of Rect.t * colour
      | Sprite of sprite

    type meta
    type item
    type t = item list ref

    val make : unit -> 'a list ref
    val clear : 'a list ref -> unit
    val push : 'a list ref -> 'a -> unit
    val iter_sorted : item list ref -> camera_layers:int64 -> f:(item -> unit) -> unit

    module R : Luma__resource.Resource.S with type t = t
  end

  module Draw : sig
    val rect : rect:Rect.t -> colour:colour -> Queue.item list ref -> unit
    val rect_screen : rect:Rect.t -> colour:colour -> Queue.item list ref -> unit
  end

  val push_rect : z:int -> rect:Rect.t -> ?layers:int64 -> colour -> Queue.item list ref -> unit

  val push_texture :
    z:int ->
    tex:texture ->
    position:Vec2.t ->
    size:Vec2.t ->
    ?layers:int64 ->
    ?texture_atlas:Texture_atlas.t ->
    ?flip_x:bool ->
    ?flip_y:bool ->
    Queue.item list ref ->
    unit ->
    unit

  val push_rect_screen :
    z:int -> ?layers:int64 -> rect:Rect.t -> colour -> Queue.item list ref -> unit

  module Camera : Camera.S
end

module Make : functor (D : Luma__driver.Driver.S) ->
  Renderer with type texture = D.texture and type colour = D.colour
