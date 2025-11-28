type spawn_request = {
  entity : Entity.t;
  name : string;
  components : Component.packed list;
}

type command =
  | Spawn of spawn_request
  | Despawn of Luma__id__Id.Entity.t
  | Insert of Luma__id.Id.Entity.t * Component.packed
  | Remove
  | Insert_resource
  | Remove_resource

type t

val create : unit -> t
val spawn : ?name:string -> ?uuid:Uuidm.t -> t -> Component.component list -> Luma__id.Id.Entity.t
val insert : t -> Luma__id.Id.Entity.t -> (module Component.S with type t = 'a) -> 'a -> unit
val flush : World.t -> t -> unit
val commands : t -> command List.t
