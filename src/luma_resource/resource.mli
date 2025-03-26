type base = ..
type error = [ `Not_found of Luma__id.Id.id | `Type_mismatch of Luma__id.Id.id ]

val error_to_string : [< `Not_found of int | `Type_mismatch of int ] -> string

module type S = sig
  type t

  val id : Luma__id.Id.Resource.t
  val of_base : base -> t
  val of_base_opt : base -> t option
  val to_base : t -> base
end

module Make (B : sig
  type inner
end) : S with type t = B.inner

type packed = Packed : (module S with type t = 'a) * 'a -> packed

val pack : 'a. (module S with type t = 'a) -> 'a -> packed
val unpack : 'a. (module S with type t = 'a) -> packed -> ('a, error) result
val id : packed -> int

module Query : sig
  (** The type ['a term] represents a resource where ['a] is the type of the resource.

      The constructor expects a first-class module where the module must satisfy the signature [S].
  *)
  type _ term = Resource : (module S with type t = 'a) -> 'a term

  (** The type ['a t] is a generalized algebraic data type (GADT) used to define a query for
      resources.

      The query is recursive and must be terminated with the [End] constructor.

      - [Res] constructs a query for a resource of type ['a term], followed by another query of type
        ['b t]. The result is a query for a tuple of type [('a * 'b) t].

      - [End] terminates the query and has type [unit t]. *)
  type _ t = Res : ('a term * 'b t) -> ('a * 'b) t | End : unit t

  val ( & ) : 'a term -> 'b t -> ('a * 'b) t
  (** Infix operator to simplify query syntax.

      Example:

      {[
        Resource.Resource_query.(Resource (module App.Time.R) & End)
      ]} *)

  val evaluate : 'a t -> (Luma__id.Id.Resource.t, packed) Hashtbl.t -> ('a, error) result
  (** [evaluate query tbl] accepts a [Resource_query.t] and an [(int, packed) Hashtbl.t], evaluates
      the query on the table, and returns a tuple of the Resources that satisfy the query. *)
end
