open Luma__math

type shape = Rigid_body.shape =
  | Circle of float
  | Aabb of Vec2.t
  | Polygon of Vec2.t array

type t
(** Packed shape storage container. *)

val create : ?initial:int -> unit -> t
(** [create initial] create a shape store with an initial capacity. *)

val len : t -> int
(** [len store] return the number of shapes currently stored. *)

val add : t -> shape -> int
(** [add store shape] insert a shape and return its handle index. *)

val remove : t -> int -> unit
(** [remove store row] remove a shape by row, swapping the last shape into its slot. *)

val clear : t -> unit
(** [clear store] remove all shapes from the store. *)

val shape_kind : t -> int -> int
(** [shape_kind store row] return the encoded shape kind at row (0=circle,1=aabb,2=polygon). *)

val is_circle : t -> int -> bool
(** [is_circle store row] return true if row is a circle. *)

val is_aabb : t -> int -> bool
(** [is_aabb store row] return true if row is an AABB. *)

val is_polygon : t -> int -> bool
(** [is_polygon store row] return true if row is a polygon. *)

val circle_radius : t -> int -> float
(** [circle_radius store row] return the circle radius at row. *)

val set_circle_radius : t -> int -> float -> unit
(** [set_circle_radius store row radius] sets the circle radius at row. *)

val aabb_half_size : t -> int -> Vec2.t
(** [aabb_half_size store row] return the AABB half-size at row. *)

val aabb_half_width : t -> int -> float
(** [aabb_half_width store row] return the AABB half-width at row. *)

val set_aabb_half_width : t -> int -> float -> unit
(** [set_aabb_half_width store row width] sets the AABB half-width at row. *)

val aabb_half_height : t -> int -> float
(** [aabb_half_height store row] return the AABB half-height at row. *)

val set_aabb_half_height : t -> int -> float -> unit
(** [set_aabb_half_height store row height] sets the AABB half-height at row. *)

val set_aabb_half_size : t -> int -> Vec2.t -> unit
(** [set_aabb_half_size] sets the AABB half_size at row. *)

val polygon_points : t -> int -> Vec2.t array
(** [polygon_points store row] return a newly allocated array of polygon points at row. *)

val polygon_points_copy : t -> int -> Vec2.t array
(** [polygon_points_copy store row] return a copy of the polygon points at row. *)

val polygon_points_x : t -> int -> float array
(** [polygon_points_x store row] returns the x value of the polygon points at row. *)

val polygon_points_y : t -> int -> float array
(** [polygon_points_y store row] returns the y value of the polygon points at row. *)

val polygon_offset : t -> int -> int
(** [polygon_offset store row] returns the starting index of this polygon in packed point arrays. *)

val polygon_count : t -> int -> int
(** [polygon_count store row] returns the number of points in this polygon. *)

val polygon_storage_x : t -> float array
(** [polygon_storage_x store] returns the packed x buffer used internally for all polygons. *)

val polygon_storage_y : t -> float array
(** [polygon_storage_y store] returns the packed y buffer used internally for all polygons. *)

val shape_at : t -> int -> shape
(** [shape_at store row] return the shape value at row. *)

module R : Luma__resource.Resource.S with type t = t
