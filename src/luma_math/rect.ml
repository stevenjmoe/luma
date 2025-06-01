type t = {
  pos : Vec2.t;
  size : Vec2.t;
}

let create ~pos ~size = { pos; size }
let x rect = Vec2.x rect.pos
let y rect = Vec2.y rect.pos
let width rect = Vec2.x rect.size
let height rect = Vec2.y rect.size
let set_x rect x = Vec2.set_x rect.pos x
let set_y rect y = Vec2.set_y rect.pos y
let center rect = Vec2.create (x rect +. (width rect /. 2.)) (y rect +. (height rect /. 2.))
let extent rect = Vec2.create (width rect /. 2.) (height rect /. 2.)
