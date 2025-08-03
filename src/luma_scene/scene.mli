type entity
type t

val from_world : string -> Luma__ecs__World.t -> t
(** [from_world name world] creates a scene from the entities and components in [world]. *)

val to_world : t -> Luma__ecs__World.t
(** [to_world scene] creates a new world populated with entities and components from [scene]. *)

val inject_into_world : t -> Luma__ecs__World.t -> Luma__ecs__World.t
(** [inject_into_world scene world] adds [scene] to [world], fails on duplicate entity UUIDs. *)

val inject_into_world_safe : t -> Luma__ecs__World.t -> Luma__ecs__World.t
(** [inject_into_world_safe scene world] adds [scene] to [world], skipping existing UUIDs. *)
