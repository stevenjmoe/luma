(** Rendering interface and systems.

    Render is responsible for turning world-space data into pixels. It owns render queues, view
    resolution, camera component to driver conversion, and viewport-aware projection utilities.

    The renderer is backend-agnostic and instantiated via {!Make}. *)

open Luma__app
open Luma__math
open Luma__image
open Luma__camera

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
    ?src:Luma__math.Rect.t option ->
    ?opacity:float ->
    ?rotation:float ->
    ?origin:Vec2.t ->
    unit ->
    unit

  val draw_circle : center_x:float -> center_y:float -> radius:float -> colour -> unit
  val draw_circle_lines : center_x:float -> center_y:float -> radius:float -> colour -> unit
  val plugin : App.t -> App.t

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

  module View : sig
    type t

    val camera_entity : t -> Luma__id__Id.Entity.t
    val camera : t -> Camera.t
    val viewport : t -> Viewport.t

    module R : Luma__resource.Resource.S with type t = t list
  end

  module Projection : sig
    val viewport_to_world_2d : View.t -> Vec2.t -> Vec2.t
    (** [viewport_to_world_2d veiew position] converts from viewport-local (render-target-local)
        coordinates to world space. The input position must already be relative to the viewport
        origin. *)

    val world_to_viewport_2d : View.t -> Vec2.t -> Vec2.t
    (** [world_to_viewport_2d view position] converts from world space to viewport-local
        (render-target-local) coordinates. *)

    val window_to_world_2d : View.t -> Vec2.t -> Vec2.t
    val world_to_window_2d : View.t -> Vec2.t -> Vec2.t
  end
end

module Make : functor (D : Luma__driver.Driver.S) (_ : Texture.S with type t = D.texture) ->
  Renderer with type texture = D.texture and type colour = D.colour
