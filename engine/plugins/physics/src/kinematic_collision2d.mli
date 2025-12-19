open Luma__math
open Luma__id.Id

(** Collision data for [Physics.move_and_collide] collisions. *)

type t

val create :
  collider:Entity.t ->
  angle:float ->
  collider_velocity:Vec2.t ->
  depth:float ->
  normal:Vec2.t ->
  position:Vec2.t ->
  remainder:Vec2.t ->
  travel:Vec2.t ->
  t

val collider : t -> Entity.t
(** [collider collision] returns the colliding body's associated entity. *)

val normal : t -> Vec2.t
(** [normal collision] returns the colliding body's normal at the point of collision. *)

val position : t -> Vec2.t
(** [position collision] returns the position of the moving body at the moment the collision
    occurred. *)

val remainder : t -> Vec2.t
(** [remainder collision] returns the moving body's remaining movement vector. *)

val travel : t -> Vec2.t
(** [travel collision] returns the movement applied to the moving body before the collision was
    resolved. *)
