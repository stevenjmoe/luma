(** Type to determine when a system will be run.

    [Startup (sys)] will run the system once when the game is first run.

    [Update (sys)] will run the system on every game tick. *)
type schedule =
  | Startup : (World.t, 'a) System.t -> schedule
  | Update : (World.t, 'a) System.t -> schedule

type t

val create : unit -> t
(** Creates a scheduler with no systems. *)

val add_system : t -> schedule -> unit
(** [add_system scheduler schedule] adds the system constructed by the [schedule] to the system. *)

val run_startup_systems : t -> World.t -> World.t
(** [run_startup_systems scheduler world] runs all the [Startup] systems before the main game loop
    starts. *)

val run_update_systems : t -> World.t -> World.t
(** [run_update_systems schedule world] runs all the [Update] systems on each iteration of the game
    loop. *)
