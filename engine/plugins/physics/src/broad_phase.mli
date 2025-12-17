type t

val create : max_bodies:int -> ?pairs_cap:int -> unit -> t
val clear : t -> unit
val update_potential_collision_pairs : t -> Grid.t -> unit
val pairs_view : t -> int Dynarray.t * int Dynarray.t
val update_broad_phase : Rb_store.t -> Grid.t -> Grid.t

module R : Luma__resource.Resource.S with type t = t
