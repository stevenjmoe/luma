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

val create : unit -> t
(** Creates a scheduler with no systems. *)

val add_system : t -> stage -> system -> unit
(** [add_system scheduler stage system] adds the system to the scheduler. *)

val run_stage : stage -> t -> World.t -> World.t
