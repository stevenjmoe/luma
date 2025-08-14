module type S = sig
  type t
  (** The unique identifier for the module. IDs are just simple integers which are generated
      sequentially. *)

  val next : unit -> t
  (** Increments and returns the next id in the sequence. *)

  val compare : t -> t -> int
  val eq : t -> t -> bool
  val of_int : int -> t
  val to_int : t -> int
end

module Entity : S
(** A module for genrating and managing unique identifiers for entities within the game. *)

module Component : S
(** A module for genrating and managing unique identifiers for components within the game. *)

module Resource : S
(** A module for genrating and managing unique identifiers for resources within the game. *)

module Asset_type : S
(** A module for genrating and managing unique identifiers for different asset types. *)

module Asset : S
(** A module for genrating and managing unique identifiers for different assets. *)

module State : S
(** A module for genrating and managing unique identifiers for different states. There should only
    be one though. *)

module Archetype : S

module Scene : S
(** A module for genrating and managing unique identifiers for different scenes. *)

module EntitySet : Set.S with type elt = Entity.t
module ComponentSet : Set.S with type elt = Component.t
module ResourceSet : Set.S with type elt = Resource.t
