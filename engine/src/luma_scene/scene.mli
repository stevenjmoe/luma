open Luma__id
open Luma__ecs
open Luma__resource
open Types

val snapshot_world : string -> World.t -> t
(** [snapshot_world name world] creates a scene from the entities and components in [world]. *)

val to_world : t -> World.t
(** [to_world scene] creates a new world populated with entities and components from [scene]. *)

val inject_into_world : t -> World.t -> World.t
(** [inject_into_world scene world] adds [scene] to [world], fails on duplicate entity UUIDs. *)

val inject_into_world_safe : t -> World.t -> World.t
(** [inject_into_world_safe scene world] adds [scene] to [world], skipping existing UUIDs. *)

module Serialize : module type of Serialize
