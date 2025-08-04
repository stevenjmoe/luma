open Luma__id
open Luma__ecs
open Luma__resource
open Luma__serialize

module Component_registry : sig
  type 'a component_entry = {
    id : Id.Component.t;
    name : string;
    instance : (module Component.S with type t = 'a);
    serializers : 'a Serialize.serializer_pack list;
  }

  type entry = Component : 'a component_entry -> entry

  type t = {
    name_to_entry : (string, entry) Hashtbl.t;
    id_to_entry : (Id.Component.t, entry) Hashtbl.t;
  }

  val create : unit -> t
  val get_entry : t -> string -> entry option

  val register_component :
    string ->
    (module Component.S with type t = 'a) ->
    'a Serialize.serializer_pack list ->
    World.t ->
    unit

  val get_json_serializer :
    'a.
    'a Serialize.serializer_pack list ->
    (module Serialize.Serializable with type repr = Serialize.Json_format.repr and type t = 'a)
    option

  module R : Resource.S with type t = t
end
