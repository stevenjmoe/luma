open Luma__types.Input_types

module type S = sig
  module Keyboard : sig
    val plugin : Luma__app.App.t -> Luma__app.App.t
    val is_key_down : Key.t -> bool
    val is_key_pressed : Key.t -> bool
    val is_key_released : Key.t -> bool
  end

  module Mouse : sig
    val is_mouse_button_pressed : Mouse_button.t -> bool
    val is_mouse_button_released : Mouse_button.t -> bool
    val is_mouse_button_up : Mouse_button.t -> bool
    val is_mouse_button_down : Mouse_button.t -> bool
    val get_mouse_x : unit -> int
    val get_mouse_y : unit -> int
    val get_mouse_position : unit -> Luma__math__Vec2.t
    val get_mouse_delta : unit -> Luma__math__Vec2.t
    val set_mouse_position : x:int -> y:int -> unit
    val set_mouse_offset : x:int -> y:int -> unit
    val set_mouse_scale : x:float -> y:float -> unit
    val get_mouse_wheel_move : unit -> float
  end
end

module Make (D : Luma__driver.Driver.S) = struct
  open Luma__ecs
  open Luma__app

  module Keyboard = struct
    let is_key_down = D.Input.Keyboard.is_key_down
    let is_key_pressed = D.Input.Keyboard.is_key_pressed
    let is_key_released = D.Input.Keyboard.is_key_released

    let begin_frame () =
      System.make ~components:End "begin_frame" (fun world _ ->
          D.Input.Keyboard.begin_frame ();
          world)

    let plugin = App.add_system (Scheduler.PreUpdate (System.WithoutResources (begin_frame ())))
  end

  module Mouse = struct
    let is_mouse_button_pressed = D.Input.Mouse.is_mouse_button_pressed
    let is_mouse_button_released = D.Input.Mouse.is_mouse_button_released
    let is_mouse_button_up = D.Input.Mouse.is_mouse_button_up
    let is_mouse_button_down = D.Input.Mouse.is_mouse_button_down
    let get_mouse_x = D.Input.Mouse.get_mouse_x
    let get_mouse_y = D.Input.Mouse.get_mouse_y
    let get_mouse_position = D.Input.Mouse.get_mouse_position
    let get_mouse_delta = D.Input.Mouse.get_mouse_position
    let set_mouse_position = D.Input.Mouse.set_mouse_position
    let set_mouse_offset = D.Input.Mouse.set_mouse_offset
    let set_mouse_scale = D.Input.Mouse.set_mouse_scale
    let get_mouse_wheel_move = D.Input.Mouse.get_mouse_wheel_move
  end
end
