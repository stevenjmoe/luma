module Id = Luma__id.Id

type t
type operation = Add of Id.Component.t | Remove of Id.Component.t

val components : t -> Id.ComponentSet.t
val entities : t -> Id.EntitySet.t
val create : Id.ComponentSet.t -> t
val hash : t -> int
val empty : unit -> t
val add : t -> Id.Entity.t -> Component.packed list -> unit
val next_hash : t -> operation -> int
val replace : t -> Id.Entity.t -> Component.packed -> unit
val query_table : t -> Id.Entity.t -> Id.Component.t -> Component.packed option
val remove_entity : t -> Id.Entity.t -> unit
