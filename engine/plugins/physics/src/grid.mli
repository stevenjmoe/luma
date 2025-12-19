type t

module Grid_cell : sig
  type t

  val len : t -> int
  val data_at : t -> int -> int
end

val create : Luma__math.Bounded2d.Aabb2d.t -> float -> t
(** [create world_bound cell_size] *)

val clear : t -> unit

val insert : t -> int -> min_x:float -> min_y:float -> max_x:float -> max_y:float -> unit
(** [insert grid body_index ~min_x ~min_y ~max_x ~max_y] *)

val occupied : t -> int Dynarray.t
(** [occupied grid] returns all cell indices that were written to this frame. Used only for fast
    clearing between rebuilds. *)

val cols : t -> int
(** [cols grid] returns the number of columns in the grid. *)

val rows : t -> int
(** [rows grid] returns the number of rows in the grid. *)

val cell_at : t -> int -> Grid_cell.t
(** [cell_at grid idx] returns the cell at the index. Raises if the index is out of range. *)

val cell_index : t -> row:int -> col:int -> int
(** [cell_index grid row col] converts 2D grid coordinates to 1D array index. *)

val iter_aabb :
  t -> min_x:float -> min_y:float -> max_x:float -> max_y:float -> f:(int -> unit) -> unit
(** [iter_aabb grid ~min_x ~min_y ~max_x ~max_y ~f] iterates over all elements whose spatial
    position intersects the axis-aligned bounding box defined by the given world-space coordinates,
    invoking [f] once for each matching element. *)

module R : Luma__resource.Resource.S with type t = t
