let flipped_horizontally_flag = 0x8000_0000
let flipped_vertically_flag = 0x4000_0000
let flipped_diagonally_flag = 0x2000_0000

let all_flip_flags =
  flipped_horizontally_flag lor flipped_vertically_flag lor flipped_diagonally_flag

type stagger_axis =
  | X
  | Y

type stagger_index =
  | Even
  | Odd

type orientation =
  | Orthogonal
  | Isometric
  | Staggered
  | Hexagonal

type render_order =
  | Right_down
  | Right_up
  | Left_down
  | Left_up

type colour = {
  alpha : int;
  red : int;
  green : int;
  blue : int;
}

type image = {
  source : string;
  width : int;
  height : int;
  transparent_colour : colour option;
}
