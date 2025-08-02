open Luma__id
open Luma__ecs
open Luma__resource

module Component_registry : sig
  type 'a component_entry = {
    id : Id.Component.t;
    name : string;
    instance : (module Component.S with type t = 'a);
  }

  type entry = Component : 'a component_entry -> entry

  type t = {
    name_to_entry : (string, entry) Hashtbl.t;
    id_to_entry : (Id.Component.t, entry) Hashtbl.t;
  }

  val create : unit -> t
  val register_component : string -> (module Component.S with type t = 'a) -> World.t -> unit

  module R : Resource.S
end
