open Luma__math

module type S = sig
  type camera
  type colour
  type texture

  val draw_rect : Rect.t -> colour -> unit

  module Window : sig
    val init : width:int -> height:int -> title:string -> unit
    val shutdown : unit -> unit
    val should_close : unit -> bool
    val get_frame_time : unit -> float
    val begin_frame : unit -> unit
    val end_frame : unit -> unit
    val begin_2d : camera -> unit
    val end_2d : unit -> unit
    val clear : colour -> unit
    val screen_width : unit -> int
    val screen_height : unit -> int
  end

  module Camera : sig
    val make :
      position:Luma__math.Vec2.t ->
      target:Luma__math.Vec2.t ->
      rotation:float ->
      zoom:float ->
      camera

    val default : unit -> camera
    val set_target : camera -> float * float -> unit
  end

  module Colour : sig
    val rgb : r:int -> g:int -> b:int -> colour
    val rgba : r:int -> g:int -> b:int -> a:int -> colour
    val white : colour
  end

  module Image : sig
    type t

    val load_image : string -> t
  end

  module Texture : sig
    type t = texture

    val width : t -> int
    val height : t -> int
    val draw_texture : t -> Rect.t -> Rect.t -> Vec2.t -> float -> colour -> unit
    val load_texture_from_image : Image.t -> t
  end
end

include S
