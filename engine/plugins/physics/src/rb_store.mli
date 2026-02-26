open Luma__math

type rigid_body = Rigid_body.t
type t

val create : ?initial:int -> unit -> t
(** [create initial] create a rigid body store with an initial capacity. *)

val len : t -> int
(** [len store] return the number of bodies currently stored. *)

val add : t -> Shape_store.t -> rigid_body -> int
(** [add store shape_store body] insert a body and return its row index. *)

val remove : t -> int -> unit
(** [remove store row] remove a body by row, swapping the last body into its slot. *)

val swap_rows : t -> int -> int -> unit
(** [swap_rows store i j] swap two rows in-place. *)

val clear : t -> unit
(** [clear store] remove all bodies from the store. *)

val iter_active : t -> (int -> unit) -> unit
(** [iter_active store f] call [f] for each active row. *)

val set_velocity : t -> int -> float -> float -> unit
(** [set_velocity store row vx vy] set linear velocity for a row. *)

val add_force : t -> int -> float -> float -> unit
(** [add_force store row fx fy] add force to the accumulator for a row. *)

val apply_impulse_at : t -> row:int -> ix:float -> iy:float -> unit
(** [apply_impulse_at store row ix iy] apply a linear impulse at a row. *)

val clear_forces_at : t -> row:int -> unit
(** [clear_forces_at store row] clear accumulated forces for a row. *)

val apply_force_at : t -> row:int -> fx:float -> fy:float -> unit
(** [apply_force_at store row fx fy] add force to a row if active and non-static. *)

val apply_gravity_at : t -> row:int -> gx:float -> gy:float -> unit
(** [apply_gravity_at store row gx gy] apply gravity to a row if active and dynamic. *)

val integrate_linear_motion_at : t -> Shape_store.t -> row:int -> dt:float -> unit
(** [integrate_linear_motion_at store shape_store row dt] integrate velocity and update bounds. *)

val bounding_box : t -> int -> Bounded2d.Aabb2d.t
(** [bounding_box store row] return the AABB for a row. *)

val shape_handle : t -> int -> int
(** [shape_handle store row] return the shape handle for a row. *)

val shape_kind : t -> int -> int
(** [shape_kind store row] return the encoded shape kind for a row. *)

val set_shape_kind : t -> int -> int -> unit
(** [set_shape_kind store row kind] set the encoded shape kind for a row. *)

val body_type : t -> int -> int
(** [body_type store row] return the encoded body type for a row. *)

val set_body_type : t -> int -> int -> unit
(** [set_body_type store row kind] set the encoded body type for a row. *)

val is_active : t -> int -> bool
(** [is_active store row] return true if the row is active. *)

val set_active : t -> int -> bool -> unit
(** [set_active store row active] set active state for a row. *)

val is_sensor : t -> int -> bool
(** [is_sensor store row] return true if the row is configured as a sensor. *)

val set_sensor : t -> int -> bool -> unit
(** [set_sensor store row sensor] set sensor state for a row. *)

val is_static : t -> int -> bool
(** [is_static store row] return true if the row is static. *)

val is_dynamic : t -> int -> bool
(** [is_dynamic store row] return true if the row is dynamic. *)

val is_kinematic : t -> int -> bool
(** [is_kinematic store row] return true if the row is kinematic. *)

val pos_x : t -> int -> float
(** [pos_x store row] return the x position for a row. *)

val pos_y : t -> int -> float
(** [pos_y store row] return the y position for a row. *)

val set_pos_x : t -> int -> float -> unit
(** [set_pos_x store row x] set the x position for a row. *)

val set_pos_y : t -> int -> float -> unit
(** [set_pos_y store row y] set the y position for a row. *)

val prev_pos_x : t -> int -> float
(** [prev_pos_x store row] return the previous x position for a row. *)

val prev_pos_y : t -> int -> float
(** [prev_pos_y store row] return the previous y position for a row. *)

val set_prev_pos_x : t -> int -> float -> unit
(** [set_prev_pos_x store row x] set the previous x position for a row. *)

val set_prev_pos_y : t -> int -> float -> unit
(** [set_prev_pos_y store row y] set the previous y position for a row. *)

val vel_x : t -> int -> float
(** [vel_x store row] return the x velocity for a row. *)

val vel_y : t -> int -> float
(** [vel_y store row] return the y velocity for a row. *)

val set_vel_x : t -> int -> float -> unit
(** [set_vel_x store row x] set the x velocity for a row. *)

val set_vel_y : t -> int -> float -> unit
(** [set_vel_y store row y] set the y velocity for a row. *)

val force_acc_x : t -> int -> float
(** [force_acc_x store row] return the x force accumulator for a row. *)

val force_acc_y : t -> int -> float
(** [force_acc_y store row] return the y force accumulator for a row. *)

val set_force_acc_x : t -> int -> float -> unit
(** [set_force_acc_x store row x] set the x force accumulator for a row. *)

val set_force_acc_y : t -> int -> float -> unit
(** [set_force_acc_y store row y] set the y force accumulator for a row. *)

val inv_mass : t -> int -> float
(** [inv_mass store row] return inverse mass for a row. *)

val set_inv_mass : t -> int -> float -> unit
(** [set_inv_mass store row inv] set inverse mass for a row. *)

val damping : t -> int -> float
(** [damping store row] return damping for a row. *)

val set_damping : t -> int -> float -> unit
(** [set_damping store row d] set damping for a row. *)

val angle : t -> int -> float
(** [angle store row] return rotation angle for a row. *)

val set_angle : t -> int -> float -> unit
(** [set_angle store row radians] set rotation angle for a row. *)

val min_x : t -> int -> float
(** [min_x store row] return min x bound for a row. *)

val min_y : t -> int -> float
(** [min_y store row] return min y bound for a row. *)

val max_x : t -> int -> float
(** [max_x store row] return max x bound for a row. *)

val max_y : t -> int -> float
(** [max_y store row] return max y bound for a row. *)

val set_min_x : t -> int -> float -> unit
(** [set_min_x store row x] set min x bound for a row. *)

val set_min_y : t -> int -> float -> unit
(** [set_min_y store row y] set min y bound for a row. *)

val set_max_x : t -> int -> float -> unit
(** [set_max_x store row x] set max x bound for a row. *)

val set_max_y : t -> int -> float -> unit
(** [set_max_y store row y] set max y bound for a row. *)

val last_seen_generation : t -> int -> int
(** [last_seen_generation store row] return last seen generation for a row. *)

val set_last_seen_generation : t -> int -> int -> unit
(** [set_last_seen_generation store row gen] set last seen generation for a row. *)

val current_generation : t -> int
(** [current_generation store] return current generation counter. *)

val set_current_generation : t -> int -> unit
(** [set_current_generation store gen] set current generation counter. *)

module R : Luma__resource.Resource.S with type t = t

module Index : sig
  type t

  module R : Luma__resource.Resource.S with type t = t

  val create : initial:int -> t
  (** [create initial] create an index with an initial capacity. *)

  val on_add : t -> entity:Luma__id.Id.Entity.t -> row:int -> unit
  (** [on_add index entity row] record a new row for an entity. *)

  val on_swap : t -> i:int -> j:int -> unit
  (** [on_swap index i j] update index after rows are swapped. *)

  val on_remove : t -> row:int -> unit
  (** [on_remove index row] remove the row from the index. *)

  val row_of_entity : t -> Luma__id.Id.Entity.t -> int option
  (** [row_of_entity index entity] lookup row for entity. *)

  val entity_at_row : t -> int -> Luma__id.Id.Entity.t
  (** [entity_at_row index row] return the entity at a row. *)
end
