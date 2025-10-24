open Luma__id
open Luma__ecs
open Luma__type_register
open Luma__resource
open Luma__serialize
open Luma__type_register.Type_register
open Luma__core
open Types

type ctx = {
  comps : Component_registry.t;
  resources : Resource_registry.t;
  version : int;
}

module Json : sig
  val serialize : t -> ctx -> (Yojson.Safe.t, Error.error) result
  val deserialize : Yojson.Safe.t -> ctx -> (t, Error.error) result
end
