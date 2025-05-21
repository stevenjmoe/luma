module Component : sig
  (** Queries can be used to retrieve entities based on the components defined in [Query.t], or
      [Filter].

      [Query.t] is a recursive GADT and needs to be terminated with [End].

      Example:

      {[
        Query.(Required (module Player_state.C) & Required (module Velocity.C) & End)
      ]}*)

  module Filter : sig
    (** A module for defining and evaluating filters over sets of components.

        A filter provided to a sytem will determine which entities should be included in the
        results, but it won't return the component data for the entity. To get the actual component
        data, include the module in the Query instead. *)

    type t =
      | With of Luma__id.Id.Component.t
      | Without of Luma__id.Id.Component.t
      | Not of t
      | And of t * t
      | Or of t * t
      | Any

    (** Infix provided to simplify filtering syntax.

        Example:

        {[
          ?filter:(Some Query.Filter.(With Player_tag.C.id & With Velocity.C.id))
        ]}*)
    val ( & ) : t -> t -> t

    (** Recursively evaluates the filter against a set of components. *)
    val matches : t -> Luma__id.Id.ComponentSet.t -> bool
  end

  type _ term =
    | Required : (module Component.S with type t = 'a) -> 'a term
    | Optional : (module Component.S with type t = 'a) -> 'a option term

  type _ t =
    | Query : ('a term * 'b t) -> ('a * 'b) t
    | End : unit t

  (** Infix provided to simplify querying syntax.

      Example:

      {[
        System.make Query.(Required (module Player_state.C) & Required (module Velocity.C) & End)
      ]}*)
  val ( & ) : 'a term -> 'b t -> ('a * 'b) t

  (** Evaluates a query over a list of archetypes, returning a list of entities and their matching
      components in a tuple. *)
  val evaluate :
    'a.
    ?filter:Filter.t ->
    'a t ->
    Archetype.t list ->
    ((Luma__id.Id.Entity.t * 'a) list, Luma__core.Error.error) result
end

module Resource : sig
  (** The type ['a term] represents a resource where ['a] is the type of the resource.

      The constructor expects a first-class module where the module must satisfy the signature [S].
  *)
  type _ term = Resource : (module Luma__resource.Resource.S with type t = 'a) -> 'a term

  (** The type ['a t] is a generalized algebraic data type (GADT) used to define a query for
      resources.

      The query is recursive and must be terminated with the [End] constructor.

      - [Res] constructs a query for a resource of type ['a term], followed by another query of type
        ['b t]. The result is a query for a tuple of type [('a * 'b) t].

      - [End] terminates the query and has type [unit t]. *)
  type _ t =
    | Res : ('a term * 'b t) -> ('a * 'b) t
    | End : unit t

  (** Infix operator to simplify query syntax.

      Example:

      {[
        Resource.Resource_query.(Resource (module App.Time.R) & End)
      ]} *)
  val ( & ) : 'a term -> 'b t -> ('a * 'b) t

  (** [evaluate query tbl] accepts a [Resource_query.t] and an [(int, packed) Hashtbl.t], evaluates
      the query on the table, and returns a tuple of the Resources that satisfy the query. *)
  val evaluate :
    'a t ->
    (Luma__id.Id.Resource.t, Luma__resource.Resource.packed) Hashtbl.t ->
    ('a, Luma__core.Error.error) result
end
