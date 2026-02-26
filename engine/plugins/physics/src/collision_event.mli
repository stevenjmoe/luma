(** Collision event data and storage. *)

open Luma__id

module Flags : sig
  type t

  val sensor : t
  val removed : t
  val has : t -> t -> bool
  val add : t -> t -> t
  val remove : t -> t -> t
end

type phase =
  | Start
  | Stay
  | Stop

val phase_to_string : phase -> string

type t

val entity_a : t -> Id.Entity.t
val entity_b : t -> Id.Entity.t
val phase : t -> phase
val is_start : t -> bool
val is_stay : t -> bool
val is_stop : t -> bool
val is_sensor : t -> bool
val is_removed : t -> bool

module Collision_events_store : sig
  type t

  val create : ?initial:int -> unit -> t

  module R : Luma__resource.Resource.S with type t = t
end

val fill_collision_events : Narrow_phase.t -> Collision_events_store.t -> unit
val iter_events : Luma__ecs__World.t -> (t -> unit) -> unit

val iter_events_for_entity :
  entity:Id.Entity.t ->
  Luma__ecs__World.t ->
  (other:Id.Entity.t -> phase -> flags:Flags.t -> unit) ->
  unit

val iter_sensor_events_for_entity :
  entity:Id.Entity.t -> Luma__ecs__World.t -> (other:Id.Entity.t -> phase -> unit) -> unit
(** [iter_sensor_events_for_entity ~entity world f] iterates only sensor events touching [entity].
*)

val iter_sensor_enters_for_entity :
  entity:Id.Entity.t -> Luma__ecs__World.t -> (other:Id.Entity.t -> unit) -> unit
(** [iter_sensor_enters_for_entity ~entity world f] iterates only [Start] sensor events touching
    [entity]. *)
