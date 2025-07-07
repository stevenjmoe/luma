type t

val create : unit -> t
(** Creates an empty [World.t]. *)

val add_entity : t -> Luma__id.Id.Entity.t
(** [add_entity world] returns the next entity id from [Id.Entity]. *)

val add_component : t -> Component.packed -> Luma__id.Id.Entity.t -> unit
(** [add_component world packed_component entity] adds a component to the world and ensures that the
    world's Archetypes are kept up to date.

    @raise Luma__core.Error.Entity_not_found if the entity hasn't been added to the game world.
    @raise Luma__core.Error.Component_not_found *)

val with_component :
  'a.
  t -> (module Component.S with type t = 'a) -> 'a -> Luma__id.Id.Entity.t -> Luma__id.Id.Entity.t
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

val archetypes : t -> (int, Archetype.t) Hashtbl.t
(** Returns world's archetypes. *)

val resources : t -> (Luma__id.Id.Resource.t, Luma__resource.Resource.packed) Hashtbl.t
(** Returns the world's resources. *)

(* TODO: This should probably accept the module and the unpacked resource and handle the complexity internally *)

val add_resource : Luma__id.Id.Resource.t -> Luma__resource.Resource.packed -> t -> t
(** [add_resource id packed world] adds a packed resource to the table of resources using the id as
    key.

    {b warning:} fails if a resource with the same [id] has already been added to the world. *)

val set_resource : Luma__id__Id.Resource.t -> Luma__resource__Resource.packed -> t -> t
(** [set_resource id packed world] adds or replaces a resource by id. *)

val has_resource : Luma__id__Id.Resource.t -> t -> bool
(** [has_resource id world] returns true if a resource with the given id has been added to the
    world. *)

val get_resource : t -> Luma__id.Id.Resource.t -> Luma__resource.Resource.packed option
(** Returns [Some packed] if found, otherwise [None] *)

val query :
  'a.
  t -> ?filter:Query.Component.Filter.t -> 'a Query.Component.t -> (Luma__id.Id.Entity.t * 'a) list
(** [query world filter query] evaluates the optional filter and required query on the world's
    archetypes and returns a [(Id.Entity.t * 'a) list] where ['a] is a tuple of components returned
    by the query. *)
