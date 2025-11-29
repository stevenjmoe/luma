open Luma__id
open Luma__resource

type t

type entity_metadata = {
  uuid : Uuidm.t;
  name : string;
}

val create : unit -> t
(** Creates an empty [World.t]. *)

val entities : t -> Id.Entity.t list
(** [entities world] returns the ids of all entities in the world. *)

val register_entity : t -> Entity.t -> string -> Id.Entity.t
(** [register_entity world entity name] *)

val add_entity : ?name:string -> ?uuid:Uuidm.t option -> t -> Id.Entity.t
(** [add_entity ?name ?uuid world] returns the next entity id from [Id.Entity]. Fails if the
    optional uuid is provided and it already exists within the world. *)

val remove_entity : t -> Id.Entity.t -> unit
(** [remove entity world entity] *)

val entity_metadata : t -> Id.Entity.t -> entity_metadata
val has_entity_uuid : t -> Uuidm.t -> bool

val add_component : t -> Component.packed -> Id.Entity.t -> unit
(** [add_component world packed_component entity] adds a component to the world and ensures that the
    world's Archetypes are kept up to date.

    @raise Luma__core.Error.Entity_not_found if the entity hasn't been added to the game world.
    @raise Luma__core.Error.Component_not_found *)

val remove_component : t -> Id.Component.t -> Id.Entity.t -> unit
(** [remove_component world component entity]*)

val with_component :
  'a. t -> (module Component.S with type t = 'a) -> 'a -> Id.Entity.t -> Luma__id.Id.Entity.t
(** [with_component world component_module component entity] provides a convenient way to add
    components to an entity without manually packing them. It automatically packs the component and
    calls [add_component] internally.

    - [world]: The world to which the component will be added.
    - [component_module]: A first-class module representing the component type. This module must
      satisfy the [Component.S] signature and specify the type of the component as ['a].
    - [component]: The component value of type ['a] to add to the entity.
    - [entity]: The entity to which the component will be added.
    - Returns: The entity ID, allowing for chaining of component additions.

    Example:
    {[
      let velocity = Velocity.zero () in
      let transform = Transform.create ~position:(Math.Vec3.create 100. 100. 100.) () in
      player
      |> World.with_component world (module Velocity.C) velocity
      |> World.with_component world (module Transform.C) transform
      |> ignore
    ]}

    @raise Luma__core.Error.Entity_not_found if the entity hasn't been added to the game world.
    @raise Luma__core.Error.Component_not_found *)

val add_components : t -> Id.Entity.t -> Component.packed list -> unit
(** [add_components world entity_id packed_components] adds a list of components to the world and
    associates them with the given entity. *)

val add_entity_with_components :
  t -> ?name:string -> ?uuid:Uuidm.t option -> Component.packed list -> Id.Entity.t
(** [add_entity_with_components world ?name ?uuid components] adds an entity with the provided
    packed components and returns the [entity_id]. *)

val archetypes : t -> (Id.Archetype.t, Archetype.t) Hashtbl.t
(** Returns world's archetypes. *)

val resources : t -> (Id.Resource.t, Resource.packed) Hashtbl.t
(** Returns the world's resources. *)

val add_resource : Luma__id.Id.Resource.t -> Resource.packed -> t -> t
(** [add_resource id packed world] adds a packed resource to the table of resources using the id as
    key.

    {b warning:} fails if a resource with the same [id] has already been added to the world. *)

val remove_resource : Id.Resource.t -> t -> unit
(** [remove_resource id world] *)

val set_resource : Id.Resource.t -> Resource.packed -> t -> t
(** [set_resource id packed world] adds or replaces a resource by id. *)

val has_resource : Id.Resource.t -> t -> bool
(** [has_resource id world] returns true if a resource with the given id has been added to the
    world. *)

val get_resource : t -> Id.Resource.t -> Resource.packed option
(** Returns [Some packed] if found, otherwise [None] *)

val query :
  'a. t -> ?filter:Query.Component.Filter.t -> 'a Query.Component.t -> (Id.Entity.t * 'a) list
(** [query world filter query] evaluates the optional filter and required query on the world's
    archetypes and returns a [(Id.Entity.t * 'a) list] where ['a] is a tuple of components returned
    by the query. *)

val get_component : t -> (module Component.S with type t = 'a) -> Id.Entity.t -> 'a option
(** Tries to retrieve the Component. Returns [Some component] if successful, otherwise None. *)

val has_component : t -> (module Component.S with type t = 'a) -> Id.Entity.t -> bool
(** Returns true if the entity has the given component. *)

module Introspect : sig
  val revision : t -> int

  val iter_entities : (Id.Entity.t -> unit) -> t -> unit
  (** [iter_entities f world] applies f to every entity in world. *)

  val entities_seq : t -> Id.Entity.t Seq.t
  (** [entities_seq world] returns a sequence of all entities. *)

  val entity_components : t -> Id.Entity.t -> Id.ComponentSet.t
  (** [entity_components world entity] returns the component set of the given entity. *)

  val get_component_packed : t -> Id.Entity.t -> Id.Component.t -> Component.packed option
  (** [get_component_packed world entity component] retrieves the packed component if present. *)

  val iter_entities_with_component : (Id.Entity.t -> unit) -> t -> Id.Component.t -> unit
  (** [iter_entities_with_component f world component] applies f to each entity with component. *)

  val resources_seq : t -> (Id.Resource.t * Resource.packed) Seq.t
end
