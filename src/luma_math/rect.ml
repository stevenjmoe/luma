type t = { 
  (** The minimum corner point *)
  min : Vec2.t;
  (** The maximum corner point *)
  max : Vec2.t }


let create ~pos ~size =  
  { min = pos; max = Vec2.Infix.(pos +.. size) }

let x rect = (Vec2.x rect.min)
let y rect = (Vec2.y rect.min)
let width rect = (Vec2.x rect.max) -. (Vec2.x rect.min)
let height rect = (Vec2.y rect.max) -. (Vec2.y rect.min)

let set_x rect x = 
  let dx = x -. Vec2.x rect.min in
  (Vec2.set_x rect.min x);
  (Vec2.set_x rect.max (Vec2.x rect.max +. dx))

let set_y rect y = 
  let dy = y -. Vec2.y rect.min in
  (Vec2.set_y rect.min y);
  (Vec2.set_y rect.max (Vec2.y rect.max +. dy))
