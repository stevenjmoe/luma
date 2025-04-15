type scheduled =
  | Startup : (World.t, 'a) System.t -> scheduled
  | PreUpdate : (World.t, 'a) System.t -> scheduled
  | Update : (World.t, 'a) System.t -> scheduled
  | PostUpdate : (World.t, 'a) System.t -> scheduled
  | PreRender : (World.t, 'a) System.t -> scheduled
  | Render : (World.t, 'a) System.t -> scheduled
  | PostRender : (World.t, 'a) System.t -> scheduled

(** Type to determine when a system will be run. *)
type stage =
  | Startup
  | PreUpdate
  | Update
  | PostUpdate
  | PreRender
  | Render
  | PostRender

type system = System : (World.t, 'a) System.t -> system
type t

val create : unit -> t
(** Creates a scheduler with no systems. *)

val add_system : t -> stage -> (World.t, 'a) System.t -> unit
(** [add_system scheduler schedule] adds the system constructed by the [schedule] to the system. *)

val add_scheduled : t -> scheduled -> unit
val run_stage : stage -> t -> World.t -> World.t
