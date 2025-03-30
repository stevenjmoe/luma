type t = { 
  (** The minimum corner point *)
  min : Vec2.t;
  (** The maximum corner point *)
  max : Vec2.t }

let check v = assert (not (Vec2.x v < 0.) && not (Vec2.y v < 0.))
let create min max =  
  check min;
  check max; 
  { min; max }
