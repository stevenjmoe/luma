open Luma__types

module type S = sig
  val plugin : Luma__app.App.t -> Luma__app.App.t
  val is_key_down : Key.t -> bool
  val is_key_pressed : Key.t -> bool
  val is_key_released : Key.t -> bool
end

module Make (D : Luma__driver.Driver.S) = struct
  open Luma__ecs
  open Luma__app

  let is_key_down = D.Input.is_key_down
  let is_key_pressed = D.Input.is_key_pressed
  let is_key_released = D.Input.is_key_released

  let begin_frame () =
    System.make ~components:End (fun world _ ->
        D.Input.begin_frame ();
        world)

  let plugin = App.add_system (Scheduler.PreUpdate (System.WithoutResources (begin_frame ())))
end
