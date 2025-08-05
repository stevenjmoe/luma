open Luma__id
open Luma__ecs
open Luma__type_register
open Luma__resource
open Luma__serialize
open Luma__type_register.Type_register
open Types

module Json : sig
  val serialize : t -> World.t -> (Yojson.Safe.t, string) result
  val deserialize : Yojson.Safe.t -> World.t -> (t, string) result
end
