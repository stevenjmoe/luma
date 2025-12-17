val iter_aabb :
  Grid.t -> min_x:float -> min_y:float -> max_x:float -> max_y:float -> f:(int -> unit) -> unit

val kinematic_toi :
  Rb_store.t -> row:int -> other:int -> dx:float -> dy:float -> (float * float * float) option
