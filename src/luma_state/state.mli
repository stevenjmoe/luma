type base = ..

module Key : sig
  type 'a t = Luma__id.Id.State.t

  (** Type equality witness. *)
  type (_, _) eq = Eq : ('a, 'a) eq

  val make : unit -> 'a t
  (** Generate a new key. *)

  val witness : 'a t -> 'b t -> ('a, 'b) eq option
  (** Check whether two keys represent the same type. Returns a type equality witness if so.

      Internally uses [Obj.magic] after verifying that the underlying keys match. This is considered
      safe under the assumption that keys are only generated via [Key.make], and equality implies
      matching phantom types. *)
end

module type STATE = sig
  type t

  val key : t Key.t
  (** Unique key identifying this state's type. *)

  val eq : t -> t -> bool
  (** Structural equality for state values. *)

  val of_base : base -> t
  val to_base : t -> base
end

(** Existential state wrapper. Stores both a module implementing [STATE] and a value of its [t]
    type. *)
type state = State : (module STATE with type t = 'a) * 'a -> state

(** Functor to create a [STATE] from a type. Adds constructors to/from the extensible [base] type.
*)
module Make (B : sig
  type inner
end) : STATE with type t = B.inner

type transition_result =
  | NoChange
  | Transitioned of {
      from : state;
      to_ : state;
    }

type state_resource = {
  previous : state option;
  current : state option;
  next : state option;
  last_result : transition_result;
}

module State_res : sig
  type t

  val create : state -> t
  (** Create initial state resource.
      - current: [s]
      - previous: [None]
      - next: [None]
      - last_result: [NoChange] *)

  val previous : t -> state option
  val current : t -> state option
  val next : t -> state option
  val last_result : t -> transition_result

  module R : Luma__resource.Resource.S with type t = state_resource
end
with type t = state_resource

val eq_state : 'a 'b. state -> state -> bool
(** Structural equality on existential states. *)

val is : (module STATE with type t = 'a) -> 'a -> state -> bool
(** Predicate to check if a state matches the given [STATE] module and value. *)

val transition_system : unit -> (Luma__ecs__World.t, unit) Luma__ecs__System.t
(** System that performs queued state transitions, to be inserted in ECS. *)

val queue_state : (module STATE with type t = 's) -> 's -> Luma__ecs__World.t -> Luma__ecs__World.t
(** Queue a state transition into the ECS world. *)

val pack : 'a. (module STATE with type t = 'a) -> 'a -> state
val unpack : 'a. (module STATE with type t = 'a) -> state -> ('a, Luma__core__Error.error) result
