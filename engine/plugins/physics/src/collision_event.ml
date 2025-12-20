open Luma__id

module Flags = struct
  type t = int

  let sensor : t = 0b0001
  let removed : t = 0b0010
  let has flags flag = flags land flag <> 0
  let add flags flag : t = flags lor flag
  let remove flags flag : t = flags land lnot flag
end

let log = Luma__core.Log.sub_log "collision_event"

type phase =
  | Start
  | Stay
  | Stop

let phase_to_string = function Start -> "Start" | Stay -> "Stay" | Stop -> "Stop"

type t = {
  entity_a : Id.Entity.t;
  entity_b : Id.Entity.t;
  phase : phase;
  flags : Flags.t;
}

let is_sensor c = Flags.(has c.flags sensor)
let is_removed c = Flags.(has c.flags removed)

module Collision_events_store = struct
  type t = {
    mutable len : int;
    mutable cap : int;
    mutable entity_a : Id.Entity.t array;
    mutable entity_b : Id.Entity.t array;
    mutable phase : int array;
    mutable flags : int array;
  }

  let create ?(initial = 128) () =
    let f v = Array.make initial v in
    {
      len = 0;
      cap = initial;
      entity_a = f Utils.sentinel_entity;
      entity_b = f Utils.sentinel_entity;
      phase = f 0;
      flags = f 0;
    }

  let ensure_capacity s need =
    if need <= s.cap then ()
    else
      let new_cap = max need (s.cap * 2) in
      let grow_int a =
        let b = Array.make new_cap 0 in
        Array.blit a 0 b 0 s.len;
        b
      in
      let grow_entity a =
        let b = Array.make new_cap Utils.sentinel_entity in
        Array.blit a 0 b 0 s.len;
        b
      in
      s.cap <- new_cap;
      s.entity_a <- grow_entity s.entity_a;
      s.entity_b <- grow_entity s.entity_b;
      s.phase <- grow_int s.phase;
      s.flags <- grow_int s.flags;
      ()

  let clear s = s.len <- 0

  let add s ~entity_a ~entity_b ~phase ~flags =
    ensure_capacity s (s.len + 1);
    let i = s.len in
    s.entity_a.(i) <- entity_a;
    s.entity_b.(i) <- entity_b;
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
      let entity_a, entity_b = entities_of_pair_key key in
      if entity_a <> Utils.sentinel_entity && entity_b <> Utils.sentinel_entity then
        (* 0 = Start. 1 = Stay *)
        let phase = if Hashtbl.mem prev_pairs key then 1 else 0 in
        let flags = 0 in
        add events ~entity_a ~entity_b ~phase ~flags)
    curr_pairs;

  Hashtbl.iter
    (fun key _ ->
      if not (Hashtbl.mem curr_pairs key) then
        let entity_a, entity_b = entities_of_pair_key key in
        if entity_a <> Utils.sentinel_entity && entity_b <> Utils.sentinel_entity then
          let phase = 2 in
          let flags = Flags.removed in
          add events ~entity_a ~entity_b ~phase ~flags)
    prev_pairs;

  let tmp = narrow.prev_pairs in
  narrow.prev_pairs <- narrow.curr_pairs;
  narrow.curr_pairs <- tmp

let iter_events (world : Luma__ecs.World.t) (f : t -> unit) =
  let open Luma__ecs in
  let open Luma__resource in
  let store_opt =
    Option.bind (World.get_resource world Collision_events_store.R.type_id) (fun packed ->
        Option.bind (Resource.unpack_opt (module Collision_events_store.R) packed) (fun r -> Some r))
  in
  match store_opt with
  | None ->
      log.warn (fun l ->
          l
            "[Collision_event.iter_events] `Collision_events_store` resource has not been added to \
             the world.")
  | Some events ->
      for i = 0 to events.len - 1 do
        let entity_a = events.entity_a.(i) in
        let entity_b = events.entity_b.(i) in
        if entity_a <> Utils.sentinel_entity && entity_b <> Utils.sentinel_entity then
          let phase =
            match events.phase.(i) with 0 -> Start | 1 -> Stay | 2 -> Stop | _ -> Stop
          in
          let flags = events.flags.(i) in
          let event = { entity_a; entity_b; phase; flags } in
          f event
      done

let iter_events_for_entity
    ~entity
    (world : Luma__ecs.World.t)
    (f : other:Id.Entity.t -> phase -> flags:Flags.t -> unit) =
  iter_events world (fun { entity_a; entity_b; phase; flags } ->
      if Id.Entity.eq entity entity_a then f ~other:entity_b phase ~flags
      else if Id.Entity.eq entity entity_b then f ~other:entity_a phase ~flags
      else ())
