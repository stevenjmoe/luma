open Luma__id

type t

val components : t -> Id.ComponentSet.t
val entities : t -> Id.EntitySet.t
val create : Id.ComponentSet.t -> t
val id : t -> Id.Archetype.t
val empty : unit -> t

val add : t -> Id.Entity.t -> Component.packed list -> unit
(** @raise Luma__core.Error.Component_not_found if the component sparse set could not be found *)

val replace : t -> Id.Entity.t -> Component.packed -> unit
(** @raise Luma__core.Error.Component_not_found if the component sparse set could not be found *)

val query_table : t -> Id.Entity.t -> Id.Component.t -> Component.packed option
val has_component : t -> Id.Component.t -> bool
val remove_entity : t -> Id.Entity.t -> unit
val pp : Format.formatter -> t -> unit
val show : t -> string
