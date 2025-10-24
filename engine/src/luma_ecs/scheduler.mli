type placement =
  | At
  | First
  | Last
  | Before of Uuidm.t
  | After of Uuidm.t

(** Type to determine when a system will be run. *)
type stage =
  | PreStartup
  | Startup
  | PostStartup
  | PreUpdate
  | StateTransition
  | Update
  | PostUpdate
  | PreRender
  | Render
  | PostRender
  | Overlay
  | Cleanup

type system =
  | System : {
      uuid : Uuidm.t;
      sys : (World.t, 'a) System.t;
      run_if : World.t -> bool;
    }
      -> system

type entry

type buckets = {
  mutable first : entry list;
  before : (Uuidm.t, entry list) Hashtbl.t;
  after : (Uuidm.t, entry list) Hashtbl.t;
  mutable last : entry list;
}

type t

val create : unit -> t
(** Creates a scheduler with no systems. *)

val add_system : t -> stage -> system -> unit
(** [add_system scheduler stage system] adds the system to the scheduler. *)

val add_in_placement : t -> stage -> (World.t -> bool) -> placement -> system -> unit
(** [add_in_placement sched stage trigger placement system] enqueues a **one-shot** [system] for
    [stage] according to [placement].

    The system will execute the first tick in which both [trigger world] and the system's own
    [run_if world] predicates are true, otherwise it will remain queued.

    @param placement

    [At] : buffer in an internal per-stage queue. When the next [add_system] is called for this
    [stage], all buffered entries are flushed before that newly added [system] (FIFO). If no
    persistent system is added before the stage runs, the buffered entries are demoted to the
    stage's [Last] bucket for that run.

    [First] : System will run before any persistent system in this [stage] when ready.

    [Last] : System will run after any persistent system in this [stage] when ready.

    [Befure u] : Append to the before list keyed by anchor UUID [u]. Entries are drained immediately
    before that anchor when ready.

    [After u] : Append to the after list keyed by anchor UUID [u]. Entries are drained immediately
    after that anchor when ready.*)

val run_stage : stage -> t -> World.t -> World.t
