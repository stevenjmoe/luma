type scheduled =
  | PreStartup : (World.t, 'a) System.t -> scheduled
  | Startup : (World.t, 'a) System.t -> scheduled
  | PostStartup : (World.t, 'a) System.t -> scheduled
  | PreUpdate : (World.t, 'a) System.t -> scheduled
  | Update : (World.t, 'a) System.t -> scheduled
  | PostUpdate : (World.t, 'a) System.t -> scheduled
  | PreRender : (World.t, 'a) System.t -> scheduled
  | Render : (World.t, 'a) System.t -> scheduled
  | PostRender : (World.t, 'a) System.t -> scheduled
  | Cleanup : (World.t, 'a) System.t -> scheduled

(** Type to determine when a system will be run. *)
type stage =
  | PreStartup
  | Startup
  | PostStartup
  | PreUpdate
  | Update
  | PostUpdate
  | PreRender
  | Render
  | PostRender
  | Cleanup

type system = System : (World.t, 'a) System.t -> system
type t

(** Creates a scheduler with no systems. *)
val create : unit -> t

(** [add_system scheduler schedule] adds the system constructed by the [schedule] to the system. *)
val add_system : t -> stage -> (World.t, 'a) System.t -> unit

val add_scheduled : t -> scheduled -> unit
val run_stage : stage -> t -> World.t -> World.t
