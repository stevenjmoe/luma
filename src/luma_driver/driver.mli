module type S = sig
  type camera
  type colour

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
  end

  module Camera : sig
    val make :
      position:float * float -> target:float * float -> rotation:float -> zoom:float -> camera

    val default : unit -> camera
    val set_target : camera -> float * float -> unit
  end

  module Colour : sig
    val rgb : r:int -> g:int -> b:int -> colour
    val rgba : r:int -> g:int -> b:int -> a:int -> colour
    val white : colour
  end
end

include S
