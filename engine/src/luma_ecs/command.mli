open Luma__id
open Luma__resource

type spawn_request = {
  entity : Entity.t;
  name : string;
  components : Component.packed list;
}

type command =
  | Spawn of spawn_request
  | Despawn of Id.Entity.t
  | Insert of Id.Entity.t * Component.packed
  | Remove of Id.Entity.t * Id.Component.t
  | Insert_resource of Resource.packed
  | Remove_resource of Id.Resource.t

type t

val create : unit -> t
val spawn : ?name:string -> ?uuid:Uuidm.t -> t -> Component.component list -> Luma__id.Id.Entity.t
val insert : t -> Luma__id.Id.Entity.t -> (module Component.S with type t = 'a) -> 'a -> unit
val remove : t -> Id.Entity.t -> Id.Component.t -> unit
val insert_resource : t -> (module Resource.S with type t = 'a) -> 'a -> unit
val remove_resource : t -> Id.Resource.t -> unit
val flush : World.t -> t -> unit
val commands : t -> command List.t
