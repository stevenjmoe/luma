module Collision_event_flags = struct
  type t = int

  let sensor : t = 0b0001
  let removed : t = 0b0010
  let has flags flag = flags land flag <> 0
  let add flags flag : t = flags lor flag
  let remove flags flag : t = flags land lnot flag
end

let log = Luma__core.Log.sub_log "collision_event"

type collider_handle = Luma__id.Id.Entity.t

type phase =
  | Start
  | Stay
  | Stop

let phase_to_string = function Start -> "Start" | Stay -> "Stay" | Stop -> "Stop"

type t = {
  a : collider_handle;
  b : collider_handle;
  phase : phase;
  event_flags : Collision_event_flags.t;
}

let is_sensor c = Collision_event_flags.(has c.event_flags sensor)
let is_removed c = Collision_event_flags.(has c.event_flags removed)

module Collision_events_store = struct
  type t = {
    mutable len : int;
    mutable cap : int;
    mutable rows_a : int array;
    mutable rows_b : int array;
    mutable phase : int array;
    mutable flags : int array;
  }

  let create ?(initial = 128) () =
    let f v = Array.make initial v in
    { len = 0; cap = initial; rows_a = f 0; rows_b = f 0; phase = f 0; flags = f 0 }

  let ensure_capacity s need =
    if need <= s.cap then ()
    else
      let new_cap = max need (s.cap * 2) in
      let grow_int a =
        let b = Array.make new_cap 0 in
        Array.blit a 0 b 0 s.len;
        b
      in
      s.cap <- new_cap;
      s.rows_a <- grow_int s.rows_a;
      s.rows_b <- grow_int s.rows_b;
      s.phase <- grow_int s.phase;
      s.flags <- grow_int s.flags;
      ()

  let clear s = s.len <- 0

  let add s ~row_a ~row_b ~phase ~flags =
    ensure_capacity s (s.len + 1);
    let i = s.len in
    s.rows_a.(i) <- row_a;
    s.rows_b.(i) <- row_b;
    s.phase.(i) <- phase;
    s.flags.(i) <- flags;
    s.len <- s.len + 1

  module R = Luma__resource.Resource.Make (struct
    type inner = t

    let name = "collision_event_store"
  end)
end

let fill_collision_events narrow events =
  let open Narrow_phase in
  let open Collision_events_store in
  clear events;
  let { prev_pairs; curr_pairs; _ } = narrow in

  Hashtbl.iter
    (fun key _ ->
      let row_a, row_b = rows_of_pair_key key in
      (* 0 = Start. 1 = Stay *)
      let phase = if Hashtbl.mem prev_pairs key then 1 else 0 in
      let flags = 0 in
      add events ~row_a ~row_b ~phase ~flags)
    curr_pairs;

  Hashtbl.iter
    (fun key _ ->
      if not (Hashtbl.mem curr_pairs key) then
        let row_a, row_b = rows_of_pair_key key in
        let phase = 2 in
        let flags = Collision_event_flags.removed in
        add events ~row_a ~row_b ~phase ~flags)
    prev_pairs;

  let tmp = narrow.prev_pairs in
  narrow.prev_pairs <- narrow.curr_pairs;
  narrow.curr_pairs <- tmp

let iter_events (events : Collision_events_store.t) (world : Luma__ecs.World.t) (f : t -> unit) =
  let open Luma__ecs in
  let open Luma__resource in
  match
    Option.bind (World.get_resource world Rb_store.Index.R.type_id) (fun packed ->
        Option.bind (Resource.unpack_opt (module Rb_store.Index.R) packed) (fun res -> Some res))
  with
  | None ->
      log.warn (fun l ->
          l
            "[Collision_event.iter_events] `Rb_store.Index` resource has not been added to the \
             world.")
  | Some index ->
      for i = 0 to events.len - 1 do
        let row_a = events.rows_a.(i) in
        let row_b = events.rows_a.(i) in
        let phase = match events.phase.(i) with 0 -> Start | 1 -> Stay | 2 -> Stop | _ -> Stop in
        let flags = events.flags.(i) in
        let entity_a = index.row_to_ent.(row_a) in
        let entity_b = index.row_to_ent.(row_b) in
        let event =
          {
            a = Luma__id.Id.Entity.of_int entity_a;
            b = Luma__id.Id.Entity.of_int entity_b;
            phase;
            event_flags = flags;
          }
        in
        f event
      done
