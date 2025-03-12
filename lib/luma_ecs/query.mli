(** Queries can be used to retrieve entities based on the components defined in [Query.t], or
    [Filter].

    [Query.t] is a recursive GADT and needs to be terminated with [End].

    Example:

    {[
      Query.(Required (module Player_state.C) & Required (module Velocity.C) & End)
    ]}*)

module Filter : sig
  (** A module for defining and evaluating filters over sets of components.

      A filter provided to a sytem will determine which entities should be included in the results,
      but it won't return the component data for the entity. To get the actual component data,
      include the module in the Query instead. *)

  type t =
    | With of Id.Component.t
    | Without of Id.Component.t
    | Not of t
    | And of t * t
    | Or of t * t
    | Any

  val ( & ) : t -> t -> t
  (** Infix provided to simplify filtering syntax.

      Example:

      {[
        ?filter:(Some Query.Filter.(With Player_tag.C.id & With Velocity.C.id))
      ]}*)

  val matches : t -> Id.ComponentSet.t -> bool
  (** Recursively evaluates the filter against a set of components. *)
end

type _ term =
  | Required : (module Component.S with type t = 'a) -> 'a term
  | Optional : (module Component.S with type t = 'a) -> 'a option term

type _ t = Query : ('a term * 'b t) -> ('a * 'b) t | End : unit t

val ( & ) : 'a term -> 'b t -> ('a * 'b) t
(** Infix provided to simplify querying syntax.

    Example:

    {[
      System.make Query.(Required (module Player_state.C) & Required (module Velocity.C) & End)
    ]}*)

val evaluate : 'a. ?filter:Filter.t -> 'a t -> Archetype.t list -> (Id.Entity.t * 'a) list
(** Evaluates a query over a list of archetypes, returning a list of entities and their matching
    components in a tuple. *)
