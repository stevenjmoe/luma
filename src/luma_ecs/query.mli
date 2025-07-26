(** Helpers for unpacking nested tuples produced by ECS queries.

    In this engine, a component product is represented as nested pairs ending in [unit], e.g.:

    - 1 component: ['a * unit]
    - 2 components: ['a * ('b * unit)]
    - 3 components: ['a * ('b * ('c * unit))]
    - ...
    - With entity id: [Id.Entity.t * (components_tuple)]

    This module provides:

    - [withN] : unpack a component tuple and call a function with N args.
    - [iterN] : iterate [(entity * components)] lists, passing N components. (the entity id is
      ignored)
    - [iter_eN] : iterate [(entity * components)] lists, passing the entity id plus N components.

    {b Ordering}: helpers preserve the input list order and do not reorder. *)
module Tuple : sig
  (** {1 Unpackers (expression style)}

      Use these when you want to destructure a components tuple inline in an expression. They
      pattern‑match and call your function; they do not build new tuples. *)

  val with1 : 'a * 'b -> ('a -> 'c) -> 'c
  (** [with1 (a, _tail) f] calls [f a]. Intended for tuples of shape ['a * unit] (i.e., 1 required
      component).*)

  val with2 : 'a * ('b * 'c) -> ('a -> 'b -> 'd) -> 'd
  (** [with2 (a, (b, _)) f] calls [f a b]. Tuple shape: ['a * ('b * unit)]. *)

  val with3 : 'a * ('b * ('c * 'd)) -> ('a -> 'b -> 'c -> 'e) -> 'e
  (** [with3 (a, (b, (c, _))) f] calls [f a b c]. Tuple shape: ['a * ('b * ('c * unit))]. *)

  val with4 : 'a * ('b * ('c * ('d * 'e))) -> ('a -> 'b -> 'c -> 'd -> 'f) -> 'f
  (** [with4 (a, (b, (c, (d, _)))) f] calls [f a b c d]. Tuple shape:
      ['a * ('b * ('c * ('d * unit)))]. *)

  val with5 : 'a * ('b * ('c * ('d * ('e * 'f)))) -> ('a -> 'b -> 'c -> 'd -> 'e -> 'g) -> 'g
  (** [with5 (a, (b, (c, (d, (e, _))))) f] calls [f a b c d e]. *)

  val with6 :
    'a * ('b * ('c * ('d * ('e * ('f * 'g))))) -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'h) -> 'h
  (** [with6 (a, (b, (c, (d, (e, (f, _)))))) f] calls [f a b c d e f]. *)

  val with7 :
    'a * ('b * ('c * ('d * ('e * ('f * ('g * 'h)))))) ->
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'i) ->
    'i
  (** [with7 (a, (b, (c, (d, (e, (f, (g, _))))))) f] calls [f a b c d e f g]. *)

  val with8 :
    'a * ('b * ('c * ('d * ('e * ('f * ('g * ('h * 'i))))))) ->
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'j) ->
    'j
  (** [with8 (a, (b, (c, (d, (e, (f, (g, (h, _)))))))) f] calls [f a b c d e f g h]. *)

  val with9 :
    'a * ('b * ('c * ('d * ('e * ('f * ('g * ('h * ('i * 'j)))))))) ->
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'k) ->
    'k
  (** [with9 (a, (b, (c, (d, (e, (f, (g, (h, (i, _))))))))) f] calls [f a b c d e f g h i]. *)

  val with10 :
    'a * ('b * ('c * ('d * ('e * ('f * ('g * ('h * ('i * ('j * 'k))))))))) ->
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'l) ->
    'l
  (** [with10 (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, _)))))))))) f] calls [f a b c d e f g h i j].
  *)

  (** {1 List iterators}

      These iterate a [(entity * components)] list and invoke your function with the components (and
      optionally the entity id). They are intended for hot paths: implement them with a single local
      [step] that pattern‑matches the nested tuple and calls [f] directly. *)

  val iter1 : ('a -> unit) -> ('b * ('a * 'c)) list -> unit
  (** Iterate a result list with 1 component; entity id is ignored.

      Input element shape: [(e, (a, _))]. *)

  val iter2 : ('a -> 'b -> unit) -> ('c * ('a * ('b * 'd))) list -> unit
  (** Iterate with 2 components; entity id is ignored.

      Input element shape: [(e, (a, (b, _)))]. *)

  val iter3 : ('a -> 'b -> 'c -> unit) -> ('d * ('a * ('b * ('c * 'e)))) list -> unit
  (** Iterate with 3 components; entity id is ignored.

      Input element shape: [(e, (a, (b, (c, _))))]. *)

  val iter4 : ('a -> 'b -> 'c -> 'd -> unit) -> ('e * ('a * ('b * ('c * ('d * 'f))))) list -> unit
  (** Iterate with 4 components; entity id is ignored.

      Input element shape: [(e, (a, (b, (c, (d, _)))))]. *)

  val iter5 :
    ('a -> 'b -> 'c -> 'd -> 'e -> unit) ->
    ('f * ('a * ('b * ('c * ('d * ('e * 'g)))))) list ->
    unit
  (** Iterate with 5 components; entity id is ignored.

      Input element shape: [(e, (a, (b, (c, (d, (e, _))))))]. *)

  val iter6 :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> unit) ->
    ('g * ('a * ('b * ('c * ('d * ('e * ('f * 'h))))))) list ->
    unit
  (** Iterate with 6 components; entity id is ignored.

      Input element shape: [(e, (a, (b, (c, (d, (e, (f, _)))))))]. *)

  val iter7 :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> unit) ->
    ('h * ('a * ('b * ('c * ('d * ('e * ('f * ('g * 'i)))))))) list ->
    unit
  (** Iterate with 7 components; entity id is ignored.

      Input element shape: [(e, (a, (b, (c, (d, (e, (f, (g, _))))))))]. *)

  val iter8 :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> unit) ->
    ('i * ('a * ('b * ('c * ('d * ('e * ('f * ('g * ('h * 'j))))))))) list ->
    unit
  (** Iterate with 8 components; entity id is ignored.

      Input element shape: [(e, (a, (b, (c, (d, (e, (f, (g, (h, _)))))))))]. *)

  val iter9 :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> unit) ->
    ('j * ('a * ('b * ('c * ('d * ('e * ('f * ('g * ('h * ('i * 'k)))))))))) list ->
    unit
  (** Iterate with 9 components; entity id is ignored.

      Input element shape: [(e, (a, (b, (c, (d, (e, (f, (g, (h, (i, _))))))))))]. *)

  val iter10 :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> unit) ->
    ('k * ('a * ('b * ('c * ('d * ('e * ('f * ('g * ('h * ('i * ('j * 'l))))))))))) list ->
    unit
  (** Iterate with 10 components; entity id is ignored.

      Input element shape: [(e, (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, _)))))))))))]. *)

  (** {1 List iterators including entity id} *)

  val iter_e1 : ('a -> 'b -> unit) -> ('a * ('b * 'c)) list -> unit
  (** Iterate with entity id and 1 component.

      Calls [f e a] for each element [(e, (a, _))]. *)

  val iter_e2 : ('a -> 'b -> 'c -> unit) -> ('a * ('b * ('c * 'd))) list -> unit
  (** Iterate with entity id and 2 components.

      Calls [f e a b] for each element [(e, (a, (b, _)))]. *)

  val iter_e3 : ('a -> 'b -> 'c -> 'd -> unit) -> ('a * ('b * ('c * ('d * 'e)))) list -> unit
  (** Iterate with entity id and 3 components.

      Calls [f e a b c] for each element [(e, (a, (b, (c, _))))]. *)

  val iter_e4 :
    ('a -> 'b -> 'c -> 'd -> 'e -> unit) -> ('a * ('b * ('c * ('d * ('e * 'f))))) list -> unit
  (** Iterate with entity id and 4 components.

      Calls [f e a b c d] for each element [(e, (a, (b, (c, (d, _)))))]. *)

  val iter_e5 :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> unit) ->
    ('a * ('b * ('c * ('d * ('e * ('f * 'g)))))) list ->
    unit

  (** Iterate with entity id and 5 components.

      Calls [f e a b c d e] for each element [(e, (a, (b, (c, (d, (e, _)))))]. *)

  val iter_e6 :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> unit) ->
    ('a * ('b * ('c * ('d * ('e * ('f * ('g * 'h))))))) list ->
    unit
  (** Iterate with entity id and 6 components.

      Calls [f e a b c d e f] for each element [(e, (a, (b, (c, (d, (e, (f, _)))))))]. *)

  val iter_e7 :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> unit) ->
    ('a * ('b * ('c * ('d * ('e * ('f * ('g * ('h * 'i)))))))) list ->
    unit

  (** Iterate with entity id and 7 components.

      Calls [f e a b c d e f g] for each element [(e, (a, (b, (c, (d, (e, (f, (g, _))))))))]. *)

  val iter_e8 :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> unit) ->
    ('a * ('b * ('c * ('d * ('e * ('f * ('g * ('h * ('i * 'j))))))))) list ->
    unit
  (** Iterate with entity id and 8 components.

      Calls [f e a b c d e f g h] for each element [(e, (a, (b, (c, (d, (e, (f, (g, (h, _)))))))))].
  *)

  val iter_e9 :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> unit) ->
    ('a * ('b * ('c * ('d * ('e * ('f * ('g * ('h * ('i * ('j * 'k)))))))))) list ->
    unit
  (** Iterate with entity id and 9 components.

      Calls [f e a b c d e f g h i] for each element
      [(e, (a, (b, (c, (d, (e, (f, (g, (h, (i, _))))))))))]. *)

  val iter_e10 :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> unit) ->
    ('a * ('b * ('c * ('d * ('e * ('f * ('g * ('h * ('i * ('j * ('k * 'l))))))))))) list ->
    unit
  (** Iterate with entity id and 10 components.

      Calls [f e a b c d e f g h i j] for each element
      [(e, (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, _)))))))))))]. *)
end

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

    val ( & ) : t -> t -> t
    (** Infix provided to simplify filtering syntax.

        Example:

        {[
          ?filter:(Some Query.Filter.(With Player_tag.C.id & With Velocity.C.id))
        ]}*)

    val matches : t -> Luma__id.Id.ComponentSet.t -> bool
    (** Recursively evaluates the filter against a set of components. *)
  end

  type _ term =
    | Required : (module Component.S with type t = 'a) -> 'a term
    | Optional : (module Component.S with type t = 'a) -> 'a option term

  type _ t =
    | Query : ('a term * 'b t) -> ('a * 'b) t
    | End : unit t

  val ( & ) : 'a term -> 'b t -> ('a * 'b) t
  (** Infix provided to simplify querying syntax.

      Example:

      {[
        System.make Query.(Required (module Player_state.C) & Required (module Velocity.C) & End)
      ]}*)

  val evaluate :
    'a.
    ?filter:Filter.t ->
    'a t ->
    Archetype.t list ->
    ((Luma__id.Id.Entity.t * 'a) list, Luma__core.Error.error) result
  (** Evaluates a query over a list of archetypes, returning a list of entities and their matching
      components in a tuple. *)
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

  val ( & ) : 'a term -> 'b t -> ('a * 'b) t
  (** Infix operator to simplify query syntax.

      Example:

      {[
        Resource.Resource_query.(Resource (module App.Time.R) & End)
      ]} *)

  val evaluate :
    'a t ->
    (Luma__id.Id.Resource.t, Luma__resource.Resource.packed) Hashtbl.t ->
    ('a, Luma__core.Error.error) result
  (** [evaluate query tbl] accepts a [Resource_query.t] and an [(int, packed) Hashtbl.t], evaluates
      the query on the table, and returns a tuple of the Resources that satisfy the query. *)
end
