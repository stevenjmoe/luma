(** Collision event data and storage. *)

open Luma__id

module Flags : sig
  type t

  val sensor : t
  val removed : t
  val has : int -> int -> bool
  val add : int -> int -> t
  val remove : int -> int -> t
end

type phase =
  | Start
  | Stay
  | Stop

val phase_to_string : phase -> string

type t

val is_sensor : t -> bool
val is_removed : t -> bool

module Collision_events_store : sig
  type t

  val create : ?initial:int -> unit -> t

  module R : Luma__resource.Resource.S with type t = t
end

val fill_collision_events : Narrow_phase.t -> Collision_events_store.t -> Rb_store.Index.t -> unit
val iter_events : Luma__ecs__World.t -> (t -> unit) -> unit

val iter_events_for_entity :
  entity:Id.Entity.t ->
  Luma__ecs__World.t ->
  (other:Id.Entity.t -> phase -> flags:Flags.t -> unit) ->
  unit
