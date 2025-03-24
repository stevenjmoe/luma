type t
type operation = Add of Luma__id.Id.Component.t | Remove of Luma__id.Id.Component.t

val components : t -> Luma__id.Id.ComponentSet.t
val entities : t -> Luma__id.Id.EntitySet.t
val create : Luma__id.Id.ComponentSet.t -> t
val hash : t -> int
val empty : unit -> t
val add : t -> Luma__id.Id.Entity.t -> Component.packed list -> unit
val next_hash : t -> operation -> int
val replace : t -> Luma__id.Id.Entity.t -> Component.packed -> unit
val query_table : t -> Luma__id.Id.Entity.t -> Luma__id.Id.Component.t -> Component.packed option
val remove_entity : t -> Luma__id.Id.Entity.t -> unit
