open Luma__id

module Component_hook : sig
  type t = entity:Id.Entity.t -> command:Command.t -> component:Component.packed -> unit
  (** [Component_hook.t] defines a function to be executed during one of the component lifecycle
      hooks: [on_insert] and [on_discard]

      - [entity] is the entity that the component is being added to.
      - [command] is the command buffer where deferred changes to the world can be queued.
      - [component] is a packed [Component] *)

  val make :
    (module Component.S with type t = 'a) ->
    (entity:Id.Entity.t -> command:Command.t -> component:'a -> unit) ->
    t
  (** [make component f] *)
end

(** [on_insert] is triggered when a component is inserted onto an entity, whether the entity already
    contains it or not.

    [on_discard] is triggered before an existing component value is discarded, either because it is
    being replaced or because the component is being removed from the entity. *)
module Component_hooks : sig
  type t = {
    on_insert : Component_hook.t option;
    on_discard : Component_hook.t option;
  }

  val empty : t
  (** Returns [t] with all records set to [None]. *)
end

type t = {
  component : Id.Component.t;
  hooks : Component_hooks.t;
}
