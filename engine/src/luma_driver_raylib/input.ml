open Luma__types.Input_types

let match_and_check pred key map = match map key with Some k -> pred k | None -> false

module Keyboard = struct
  let map_key : Key.t -> Raylib.Key.t option = function
    | Null -> Some Raylib.Key.Null
    | Apostrophe -> Some Raylib.Key.Apostrophe
    | Comma -> Some Raylib.Key.Comma
    | Minus -> Some Raylib.Key.Minus
    | Period -> Some Raylib.Key.Period
    | Slash -> Some Raylib.Key.Slash
    | Zero -> Some Raylib.Key.Zero
    | One -> Some Raylib.Key.One
    | Two -> Some Raylib.Key.Two
    | Three -> Some Raylib.Key.Three
    | Four -> Some Raylib.Key.Four
    | Five -> Some Raylib.Key.Five
    | Six -> Some Raylib.Key.Six
    | Seven -> Some Raylib.Key.Seven
    | Eight -> Some Raylib.Key.Eight
    | Nine -> Some Raylib.Key.Nine
    | Semicolon -> Some Raylib.Key.Semicolon
    | Equal -> Some Raylib.Key.Equal
    | A -> Some Raylib.Key.A
    | B -> Some Raylib.Key.B
    | C -> Some Raylib.Key.C
    | D -> Some Raylib.Key.D
    | E -> Some Raylib.Key.E
    | F -> Some Raylib.Key.F
    | G -> Some Raylib.Key.G
    | H -> Some Raylib.Key.H
    | I -> Some Raylib.Key.I
    | J -> Some Raylib.Key.J
    | K -> Some Raylib.Key.K
    | L -> Some Raylib.Key.L
    | M -> Some Raylib.Key.M
    | N -> Some Raylib.Key.N
    | O -> Some Raylib.Key.O
    | P -> Some Raylib.Key.P
    | Q -> Some Raylib.Key.Q
    | R -> Some Raylib.Key.R
    | S -> Some Raylib.Key.S
    | T -> Some Raylib.Key.T
    | U -> Some Raylib.Key.U
    | V -> Some Raylib.Key.V
    | W -> Some Raylib.Key.W
    | X -> Some Raylib.Key.X
    | Y -> Some Raylib.Key.Y
    | Z -> Some Raylib.Key.Z
    | Left_bracket -> Some Raylib.Key.Left_bracket
    | Backslash -> Some Raylib.Key.Backslash
    | Right_bracket -> Some Raylib.Key.Right_bracket
    | Grave -> Some Raylib.Key.Grave
    | Space -> Some Raylib.Key.Space
    | Escape -> Some Raylib.Key.Escape
    | Enter -> Some Raylib.Key.Enter
    | Tab -> Some Raylib.Key.Tab
    | Backspace -> Some Raylib.Key.Backspace
    | Insert -> Some Raylib.Key.Insert
    | Delete -> Some Raylib.Key.Delete
    | Right -> Some Raylib.Key.Right
    | Left -> Some Raylib.Key.Left
    | Down -> Some Raylib.Key.Down
    | Up -> Some Raylib.Key.Up
    | Page_up -> Some Raylib.Key.Page_up
    | Page_down -> Some Raylib.Key.Page_down
    | Home -> Some Raylib.Key.Home
    | End -> Some Raylib.Key.End
    | Caps_lock -> Some Raylib.Key.Caps_lock
    | Scroll_lock -> Some Raylib.Key.Scroll_lock
    | Num_lock -> Some Raylib.Key.Num_lock
    | Print_screen -> Some Raylib.Key.Print_screen
    | Pause -> Some Raylib.Key.Pause
    | F1 -> Some Raylib.Key.F1
    | F2 -> Some Raylib.Key.F2
    | F3 -> Some Raylib.Key.F3
    | F4 -> Some Raylib.Key.F4
    | F5 -> Some Raylib.Key.F5
    | F6 -> Some Raylib.Key.F6
    | F7 -> Some Raylib.Key.F7
    | F8 -> Some Raylib.Key.F8
    | F9 -> Some Raylib.Key.F9
    | F10 -> Some Raylib.Key.F10
    | F11 -> Some Raylib.Key.F11
    | F12 -> Some Raylib.Key.F12
    | Left_shift -> Some Raylib.Key.Left_shift
    | Left_control -> Some Raylib.Key.Left_control
    | Left_alt -> Some Raylib.Key.Left_alt
    | Left_super -> Some Raylib.Key.Left_super
    | Right_shift -> Some Raylib.Key.Right_shift
    | Right_control -> Some Raylib.Key.Right_control
    | Right_alt -> Some Raylib.Key.Right_alt
    | Right_super -> Some Raylib.Key.Right_super
    | Kb_menu -> Some Raylib.Key.Kb_menu
    | Kp_0 -> Some Raylib.Key.Kp_0
    | Kp_1 -> Some Raylib.Key.Kp_1
    | Kp_2 -> Some Raylib.Key.Kp_2
    | Kp_3 -> Some Raylib.Key.Kp_3
    | Kp_4 -> Some Raylib.Key.Kp_4
    | Kp_5 -> Some Raylib.Key.Kp_5
    | Kp_6 -> Some Raylib.Key.Kp_6
    | Kp_7 -> Some Raylib.Key.Kp_7
    | Kp_8 -> Some Raylib.Key.Kp_8
    | Kp_9 -> Some Raylib.Key.Kp_9
    | Kp_decimal -> Some Raylib.Key.Kp_decimal
    | Kp_divide -> Some Raylib.Key.Kp_divide
    | Kp_multiply -> Some Raylib.Key.Kp_multiply
    | Kp_subtract -> Some Raylib.Key.Kp_subtract
    | Kp_add -> Some Raylib.Key.Kp_add
    | Kp_enter -> Some Raylib.Key.Kp_enter
    | Kp_equal -> Some Raylib.Key.Kp_equal
    | Back -> Some Raylib.Key.Back
    | Menu -> Some Raylib.Key.Menu
    | Volume_up -> Some Raylib.Key.Volume_up
    | Volume_down -> Some Raylib.Key.Volume_down

  let begin_frame () = ()
  let is_key_down (key : Key.t) = match_and_check Raylib.is_key_down key map_key
  let is_key_pressed (key : Key.t) = match_and_check Raylib.is_key_pressed key map_key
  let is_key_released (key : Key.t) = match_and_check Raylib.is_key_released key map_key
end

module Mouse = struct
  open Luma__types.Input_types.Mouse_button
  open Raylib.MouseButton

  let map_key : Mouse_button.t -> Raylib.MouseButton.t option = function
    | Left -> Some Left
    | Right -> Some Right
    | Middle -> Some Middle
    | Side -> Some Side
    | Extra -> Some Extra
    | Forward -> Some Forward
    | Back -> Some Back

  let is_mouse_button_pressed (button : Mouse_button.t) =
    match_and_check Raylib.is_mouse_button_pressed button map_key

  let is_mouse_button_released button =
    match_and_check Raylib.is_mouse_button_released button map_key

  let is_mouse_button_up button = match_and_check Raylib.is_mouse_button_up button map_key
  let is_mouse_button_down button = match_and_check Raylib.is_mouse_button_down button map_key
  let get_mouse_x = Raylib.get_mouse_x
  let get_mouse_y = Raylib.get_mouse_y

  let get_mouse_position () =
    let pos = Raylib.get_mouse_position () in
    Luma__math.Vec2.create (Raylib.Vector2.x pos) (Raylib.Vector2.y pos)

  let get_mouse_delta () =
    let delta = Raylib.get_mouse_delta () in
    Luma__math.Vec2.create (Raylib.Vector2.x delta) (Raylib.Vector2.y delta)

  let set_mouse_position ~x ~y = Raylib.set_mouse_position x y
  let set_mouse_offset ~x ~y = Raylib.set_mouse_offset x y
  let set_mouse_scale ~x ~y = Raylib.set_mouse_scale x y
  let get_mouse_wheel_move = Raylib.get_mouse_wheel_move
end
