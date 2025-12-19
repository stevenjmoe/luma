(** Broad phase storage.

    Maintains an ephemeral store of potential collision pairs each frame. *)

type t

val create : max_bodies:int -> ?pairs_cap:int -> unit -> t
(** [create max_bodies ?pairs_cap unit] creates and returns the storage. [pairs_cap] ensures the
    underlying storage is initialised with the given capacity. *)

val update_broad_phase : Rb_store.t -> Grid.t -> unit
(** [update_broad_phase rb_store grid] clears the grid before re-populating it with data from the
    store. It is intended to run once per step before [update_potential_collision_pairs]. [grid] is
    mutated in place. *)

val update_potential_collision_pairs : t -> Grid.t -> unit
(** [update_potential_collision_pairs bp grid] iterates the grid to find potential collision pairs
    and updates the internal broad phase storage. If called directly, it should be called after
    [update_broad_phase]. However, it is usually a better idea just to call [step], which calls both
    in sequence. *)

val step : Rb_store.t -> Grid.t -> t -> unit
(** [step rb_store grid bp] updates the broad phase storage with the current frame's potential
    collision pairs. It calls [update_broad_phase] and [update_potential_collision_pairs] in
    sequence. *)

val pairs_view : t -> int Dynarray.t * int Dynarray.t
(** [pairs_view bp] returns a tuple of the underlying pair id arrays. *)

module R : Luma__resource.Resource.S with type t = t
