open Luma__math
open Luma__ecs

module type S = sig
  type camera

  module Camera : sig
    type t = {
      camera : camera;
      active : bool;
    }

    module C : Component.S with type t = t
  end

  val default : unit -> camera
  val make : offset:Vec2.t -> target:Vec2.t -> rotation:float -> zoom:float -> unit -> camera
  val target : camera -> Vec2.t
  val offset : camera -> Vec2.t
  val zoom : camera -> float
  val rotation : camera -> float
  val set_target : camera -> Vec2.t -> unit
  val set_offset : camera -> Vec2.t -> unit
  val set_zoom : camera -> float -> unit
  val set_rotation : camera -> float -> unit
end

module Make : functor (D : Luma__driver.Driver.S) -> S with type camera = D.camera
