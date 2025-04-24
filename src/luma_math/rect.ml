type t = { 
  (** The minimum corner point *)
  min : Vec2.t;
  (** The maximum corner point *)
  max : Vec2.t }

let check v = assert (not (Vec2.x v < 0.) && not (Vec2.y v < 0.))

(** [create min max]  *)
let create min max =  
  check min;
  check max; 
  { min; max }

let x rect = (Vec2.x rect.min)
let y rect = (Vec2.y rect.min)
let width rect = (Vec2.x rect.max) -. (Vec2.x rect.min)
let height rect = (Vec2.y rect.max) -. (Vec2.y rect.min)
