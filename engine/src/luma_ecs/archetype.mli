type t

type operation =
  | Add of Luma__id.Id.Component.t
  | Remove of Luma__id.Id.Component.t

val components : t -> Luma__id.Id.ComponentSet.t
val entities : t -> Luma__id.Id.EntitySet.t
val create : Luma__id.Id.ComponentSet.t -> t
val hash : t -> int
val empty : unit -> t

(** @raise Luma__core.Error.Component_not_found if the component sparse set could not be found *)
val add : t -> Luma__id.Id.Entity.t -> Component.packed list -> unit

val next_hash : t -> operation -> int

(** @raise Luma__core.Error.Component_not_found if the component sparse set could not be found *)
val replace : t -> Luma__id.Id.Entity.t -> Component.packed -> unit

val query_table : t -> Luma__id.Id.Entity.t -> Luma__id.Id.Component.t -> Component.packed option
val remove_entity : t -> Luma__id.Id.Entity.t -> unit
val pp : Format.formatter -> t -> unit
val show : t -> string
