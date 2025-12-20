type t

val default : unit -> t
val clear : t -> unit
val on_floor : t -> bool
val on_wall : t -> bool
val on_ceiling : t -> bool
val floor_normal : t -> Luma__math__Vec2.t
val set_is_on_floor : t -> bool -> unit
val set_is_on_wall : t -> bool -> unit
val set_is_on_ceiling : t -> bool -> unit
val set_floor_normal_x : t -> float -> unit
val set_floor_normal_y : t -> float -> unit

module C : Luma__ecs.Component.S with type t = t
