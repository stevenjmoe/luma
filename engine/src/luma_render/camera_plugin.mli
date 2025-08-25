open Luma__app

module type S = sig
  type camera

  val plugin : App.t -> App.t
end

module Make : functor
  (D : Luma__driver.Driver.S)
  (Camera : Camera_component.S with type camera = D.camera)
  -> S with type camera = D.camera
