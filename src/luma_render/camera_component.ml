open Luma__ecs
open Luma__math

module type S = sig
  type camera

  module Component : sig
    type t = {
      camera : camera;
      active : bool;
    }

    module C : Luma__ecs.Component.S with type t = t
  end

  val default : unit -> camera
  val make : offset:Vec2.t -> target:Vec2.t -> rotation:float -> zoom:float -> unit -> camera
  val set_target : camera -> Vec2.t -> unit
  val set_offset : camera -> Vec2.t -> unit
  val set_zoom : camera -> float -> unit
  val set_rotation : camera -> float -> unit
end

module Make (D : Luma__driver.Driver.S) : S with type camera = D.camera = struct
  type camera = D.camera

  [%%component
  module Component = struct
    type t = {
      camera : camera;
      active : bool;
    }
  end]

  let default = D.Camera.default
  let make = D.Camera.make
  let set_target = D.Camera.set_target
  let set_offset = D.Camera.set_offset
  let set_zoom = D.Camera.set_zoom
  let set_rotation = D.Camera.set_rotation
end
