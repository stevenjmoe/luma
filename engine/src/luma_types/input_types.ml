module Key = struct
  type t =
    | Null
    | Apostrophe
    | Comma
    | Minus
    | Period
    | Slash
    | Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Semicolon
    | Equal
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z
    | Left_bracket
    | Backslash
    | Right_bracket
    | Grave
    | Space
    | Escape
    | Enter
    | Tab
    | Backspace
    | Insert
    | Delete
    | Right
    | Left
    | Down
    | Up
    | Page_up
    | Page_down
    | Home
    | End
    | Caps_lock
    | Scroll_lock
    | Num_lock
    | Print_screen
    | Pause
    | F1
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | F10
    | F11
    | F12
    | Left_shift
    | Left_control
    | Left_alt
    | Left_super
    | Right_shift
    | Right_control
    | Right_alt
    | Right_super
    | Kb_menu
    | Kp_0
    | Kp_1
    | Kp_2
    | Kp_3
    | Kp_4
    | Kp_5
    | Kp_6
    | Kp_7
    | Kp_8
    | Kp_9
    | Kp_decimal
    | Kp_divide
    | Kp_multiply
    | Kp_subtract
    | Kp_add
    | Kp_enter
    | Kp_equal
    | Back
    | Menu
    | Volume_up
    | Volume_down

  let to_int = function
    | Null -> 0
    | Apostrophe -> 39
    | Comma -> 44
    | Minus -> 45
    | Period -> 46
    | Slash -> 47
    | Zero -> 48
    | One -> 49
    | Two -> 50
    | Three -> 51
    | Four -> 52
    | Five -> 53
    | Six -> 54
    | Seven -> 55
    | Eight -> 56
    | Nine -> 57
    | Semicolon -> 59
    | Equal -> 61
    | A -> 65
    | B -> 66
    | C -> 67
    | D -> 68
    | E -> 69
    | F -> 70
    | G -> 71
    | H -> 72
    | I -> 73
    | J -> 74
    | K -> 75
    | L -> 76
    | M -> 77
    | N -> 78
    | O -> 79
    | P -> 80
    | Q -> 81
    | R -> 82
    | S -> 83
    | T -> 84
    | U -> 85
    | V -> 86
    | W -> 87
    | X -> 88
    | Y -> 89
    | Z -> 90
    | Left_bracket -> 91
    | Backslash -> 92
    | Right_bracket -> 93
    | Grave -> 96
    | Space -> 32
    | Escape -> 256
    | Enter -> 257
    | Tab -> 258
    | Backspace -> 259
    | Insert -> 260
    | Delete -> 261
    | Right -> 262
    | Left -> 263
    | Down -> 264
    | Up -> 265
    | Page_up -> 266
    | Page_down -> 267
    | Home -> 268
    | End -> 269
    | Caps_lock -> 280
    | Scroll_lock -> 281
    | Num_lock -> 282
    | Print_screen -> 283
    | Pause -> 284
    | F1 -> 290
    | F2 -> 291
    | F3 -> 292
    | F4 -> 293
    | F5 -> 294
    | F6 -> 295
    | F7 -> 296
    | F8 -> 297
    | F9 -> 298
    | F10 -> 299
    | F11 -> 300
    | F12 -> 301
    | Left_shift -> 340
    | Left_control -> 341
    | Left_alt -> 342
    | Left_super -> 343
    | Right_shift -> 344
    | Right_control -> 345
    | Right_alt -> 346
    | Right_super -> 347
    | Kb_menu -> 348
    | Kp_0 -> 320
    | Kp_1 -> 321
    | Kp_2 -> 322
    | Kp_3 -> 323
    | Kp_4 -> 324
    | Kp_5 -> 325
    | Kp_6 -> 326
    | Kp_7 -> 327
    | Kp_8 -> 328
    | Kp_9 -> 329
    | Kp_decimal -> 330
    | Kp_divide -> 331
    | Kp_multiply -> 332
    | Kp_subtract -> 333
    | Kp_add -> 334
    | Kp_enter -> 335
    | Kp_equal -> 336
    | Back -> 4
    | Menu -> 82
    | Volume_up -> 24
    | Volume_down -> 25

  let of_int i = Volume_up
end

module Mouse_button = struct
  type t =
    | Left
    | Right
    | Middle
    | Side
    | Extra
    | Forward
    | Back

  (*TODO: *)
  let to_int x = 0
  let of_int i = Left
end
