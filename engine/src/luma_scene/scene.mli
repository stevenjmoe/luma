open Luma__asset
open Luma__app
open Luma__core
open Luma__id
open Luma__ecs
open Luma__resource
open Types

(** Scene operations for snapshotting, injecting, and serializing worlds. *)
module type S = sig
  val snapshot_world : string -> World.t -> t
  val inject_into_world : t -> World.t -> World.t
  val inject_into_world_safe : t -> World.t -> World.t
  val to_world : t -> World.t
  val write : t -> World.t -> unit
  val read : string -> string
  val add_plugin : App.t -> App.t
  val ctx_of_world : World.t -> (Serialize.ctx, Error.error) result

  module A : Asset.S with type t = Types.t
  module Serialize : module type of Serialize
end

module Make : functor (D : Luma__driver.Driver.S) -> S
