(** Rendering interface and systems.

    Render is responsible for turning world-space data into pixels. It owns render queues, view
    resolution, camera component to driver conversion, and viewport-aware projection utilities.

    The renderer is backend-agnostic and instantiated via {!Make}.

    Installing the renderer plugin sets up a default render pipeline:

    - a per-frame render queue is created and cleared
    - built-in extract systems enqueue draw commands for:
    - entities with both a [Sprite] and [Transform] component.
    - [Shape] components (rects, circles, lines)
    - active camera views are resolved.
    - the queue is rendered once per view *)

open Luma__app
open Luma__math
open Luma__image

module Projection_pure : sig
  val window_to_viewport_pos : viewport_pos:Vec2.t -> Vec2.t -> Vec2.t
  val viewport_to_window_pos : viewport_pos:Vec2.t -> Vec2.t -> Vec2.t
end

module type Renderer = sig
  (** The renderer is responsible for:
      - collecting draw commands into a queue
      - resolving active views (camera + viewport pairs)
      - executing rendering for each view
      - performing all world/screen/viewport projection

      Each camera produces one view, which renders the same queue into a specific viewport. *)

  type texture
  type colour

  module Shape : Shape.S with type colour = colour

  val draw_rect : Rect.t -> colour -> unit
  (** [draw_rect rect colour] draws a rectangle immediately in the current render context, bypassing
      the render queue.

      Immediate drawing is not queued and is not automatically scoped to any view or viewport. It
      should generally only be used for debug or driver-level drawing. *)

  val draw_rect_lines : Rect.t -> float -> colour -> unit
  (** [draw_rect_lines rect line_thickness colour] draws rectangle lines immediately in the current
      render context, bypassing the render queue.

      Immediate drawing is not queued and is not automatically scoped to any view or viewport. It
      should generally only be used for debug or driver-level drawing. *)

  val draw_texture :
    size:Luma__math.Vec2.t ->
    position:Vec2.t ->
    ?flip_x:bool ->
    ?flip_y:bool ->
    ?src:Luma__math.Rect.t option ->
    ?opacity:float ->
    ?rotation:float ->
    ?origin:Vec2.t ->
    texture ->
    unit
  (** [draw_texture size position ?flip_x ?flip_y ?src ?opacity ?rotation ?origin texture] draws a
      texture immediately in the current render context, bypassing the render queue.

      Immediate drawing is not queued and is not automatically scoped to any view or viewport. It
      should generally only be used for debug or driver-level drawing. *)

  val draw_circle : center_x:float -> center_y:float -> radius:float -> colour -> unit
  (** [draw_circle center_x center_y radius colour] draws a circle immediately in the current render
      context, bypassing the render queue.

      Immediate drawing is not queued and is not automatically scoped to any view or viewport. It
      should generally only be used for debug or driver-level drawing. *)

  val draw_circle_lines : center_x:float -> center_y:float -> radius:float -> colour -> unit
  (** [draw_circle_lines center_x center_y radius colour] draws circle lines immediately in the
      current render context, bypassing the render queue.

      Immediate drawing is not queued and is not automatically scoped to any view or viewport. It
      should generally only be used for debug or driver-level drawing. *)

  val plugin : App.t -> App.t

  module Queue : sig
    (** Render command queue.

        The queue is populated by built-in extract systems (eg. [Sprite] + [Transform] and [Shape]
        containing entities, as well as by user code via [push_*] helpers.

        Commands are filtered by camera layers and sorted by z-order before rendering. *)

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
    (** Mutable render queue resource. Cleared at the beginning of each frame. *)

    val make : unit -> t

    module R : Luma__resource.Resource.S with type t = t
  end

  module Draw : sig
    (** Convenience helpers for enqueing common draw commands. *)

    val rect : rect:Rect.t -> colour:colour -> Queue.item list ref -> unit
    val rect_screen : rect:Rect.t -> colour:colour -> Queue.item list ref -> unit
  end

  val push_rect : z:int -> rect:Rect.t -> ?layers:int64 -> colour -> Queue.item list ref -> unit
  (** [push_rect z rect ?layers colour queue] enqueues a world-space rectangle draw command. *)

  val push_rect_lines :
    z:int ->
    rect:Rect.t ->
    ?layers:int64 ->
    ?line_thickness:float ->
    colour ->
    Queue.item list ref ->
    unit
  (** [push_rect_lines z rect ?layers ?line_thickness colour queue] enqueues world-space rectangle
      lines draw command. *)

  val push_circle :
    z:int -> circle:Primitives.Circle.t -> ?layers:int64 -> colour -> Queue.item list ref -> unit
  (** [push_circle z circle ?layers colour queue] enqueues a world-space circle draw command. *)

  val push_circle_lines :
    z:int -> circle:Primitives.Circle.t -> ?layers:int64 -> colour -> Queue.item list ref -> unit
  (** [push_circle z circle ?layers colour queue] enqueues world-space circle lines draw command. *)

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
    unit
  (** [push_texture z tex position size ?layers ?src ?flip_x ?flip_y ?opacity ?rotation ?origin
       queue] enqueues a world-space texture draw command. *)

  val push_rect_screen :
    z:int -> ?layers:int64 -> rect:Rect.t -> colour -> Queue.item list ref -> unit
  (** [push_rect_screen z layers rect colour queue] enqueues a screen-space rectangle draw command.

      Screen-space commands are rendered before world-space commands for each view and are not
      affected by the camera transform. *)

  module Projection : sig
    (** Coordinate space conversion utilities.

        All conversions are defined relative to a resolved {!View}, which encapsulates both camera
        and viewport information. *)

    val viewport_to_world_2d : View.t -> Vec2.t -> Vec2.t
    (** [viewport_to_world_2d view position] converts from viewport-local (render-target-local)
        coordinates to world space. The input position must already be relative to the viewport
        origin. *)

    val world_to_viewport_2d : View.t -> Vec2.t -> Vec2.t
    (** [world_to_viewport_2d view position] converts from world space to viewport-local
        (render-target-local) coordinates. *)

    val window_to_world_2d : View.t -> Vec2.t -> Vec2.t
    (**[window_to_world_2d view position] converts from window coordinates to world space. *)

    val world_to_window_2d : View.t -> Vec2.t -> Vec2.t
    (**[world_to_window_2d view position] converts from world space to window coordinates. *)
  end
end

module Make : functor (D : Luma__driver.Driver.S) (_ : Texture.S with type t = D.texture) ->
  Renderer with type texture = D.texture and type colour = D.colour
