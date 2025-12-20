let log = Luma__core.Log.sub_log "luma.scheduler"

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

let stage_to_string = function
  | PreStartup -> "PreStartup"
  | Startup -> "Startup"
  | PostStartup -> "PostStartup"
  | PreUpdate -> "PreUpdate"
  | StateTransition -> "StateTransition"
  | Update -> "Update"
  | PostUpdate -> "PostUpdate"
  | PreRender -> "PreRender"
  | Render -> "Render"
  | PostRender -> "PostRender"
  | Overlay -> "Overlay"
  | Cleanup -> "Cleanup"

type system =
  | System : {
      uuid : Uuidm.t;
      sys : (World.t, _) System.t;
      run_if : World.t -> bool;
    }
      -> system

type trigger = World.t -> bool

type entry = {
  trigger : trigger;
  system : system;
}

type buckets = {
  mutable first : entry list;
  before : (Uuidm.t, entry list) Hashtbl.t;
  after : (Uuidm.t, entry list) Hashtbl.t;
  mutable last : entry list;
}

type t = {
  systems : (stage, system list) Hashtbl.t;
  once : (stage, buckets) Hashtbl.t;
  pending_at : (stage, entry list) Hashtbl.t;
}

type placement =
  | At
  | First
  | Last
  (* TODO: *)
  | Before of Uuidm.t
  | After of Uuidm.t

let buckets_or_default sched stage =
  Hashtbl.find_opt sched.once stage
  |> Option.value
       ~default:{ first = []; last = []; before = Hashtbl.create 7; after = Hashtbl.create 7 }

let create () =
  log.info (fun log -> log "Creating scheduler.");
  let systems = Hashtbl.create 16 in
  let once = Hashtbl.create 16 in
  let pending_at = Hashtbl.create 16 in
  List.iter
    (fun stage ->
      Hashtbl.add systems stage [];
      Hashtbl.add pending_at stage [])
    [
      PreStartup;
      Startup;
      PostStartup;
      PreUpdate;
      StateTransition;
      Update;
      PostUpdate;
      PreRender;
      Render;
      PostRender;
      Overlay;
      Cleanup;
    ];
  { systems; once; pending_at }

let add_system (sched : t) (stage : stage) (System { uuid; _ } as system : system) =
  let systems = Hashtbl.find sched.systems stage in
  Hashtbl.replace sched.systems stage (system :: systems);

  let pending = Hashtbl.find sched.pending_at stage in
  match pending with
  | [] -> ()
  | entries ->
      let buckets = buckets_or_default sched stage in
      let cur = Hashtbl.find_opt buckets.before uuid |> Option.value ~default:[] in

      (* flush At -> BEFORE this system *)
      Hashtbl.replace buckets.before uuid (cur @ entries);

      Hashtbl.replace sched.once stage buckets;
      Hashtbl.replace sched.pending_at stage []

let add_in_placement sched stage trigger placement system =
  let e = { trigger; system } in
  match placement with
  | At ->
      let p = Hashtbl.find sched.pending_at stage in
      Hashtbl.replace sched.pending_at stage (p @ [ e ])
  | First ->
      let buckets = buckets_or_default sched stage in
      buckets.first <- buckets.first @ [ e ];
      Hashtbl.replace sched.once stage buckets
  | Last ->
      let buckets = buckets_or_default sched stage in
      buckets.last <- buckets.last @ [ e ];
      Hashtbl.replace sched.once stage buckets
  | Before u ->
      let buckets = buckets_or_default sched stage in
      let entries = Hashtbl.find_opt buckets.before u |> Option.value ~default:[] in
      let entries' = entries @ [ e ] in
      Hashtbl.replace buckets.before u entries';
      Hashtbl.replace sched.once stage buckets
  | After u ->
      let buckets = buckets_or_default sched stage in
      let entries = Hashtbl.find_opt buckets.after u |> Option.value ~default:[] in
      let entries' = entries @ [ e ] in
      Hashtbl.replace buckets.after u entries';
      Hashtbl.replace sched.once stage buckets

let run_system (world : World.t) cmd (System { sys = system; run_if; _ }) : World.t =
  if run_if world then
    let archetypes = World.archetypes world |> Hashtbl.to_seq_values |> List.of_seq in
    match system with
    | System.WithoutResources s -> (
        match Query.Component.evaluate ~filter:s.filter s.components_query archetypes with
        | Ok matching_entities -> s.run world cmd matching_entities
        | Error e ->
            let msg = Format.asprintf "Failed to run system: %s. %a" s.name Luma__core.Error.pp e in
            log.error (fun l -> l "%s" msg);
            Luma__core.Error.system_run_exn s.name msg)
    | System.WithResources s -> (
        match Query.Component.evaluate ~filter:s.filter s.components_query archetypes with
        | Ok matching_entities -> (
            match Query.Resource.evaluate s.resources_query (World.resources world) with
            | Ok resource_value -> s.run world cmd matching_entities resource_value
            | Error e ->
                let msg =
                  Format.asprintf "Failed to evaluate Resource query for system: %s. %a" s.name
                    Luma__core.Error.pp e
                in
                log.error (fun l -> l "%s" msg);
                Luma__core.Error.system_run_exn s.name msg)
        | Error e ->
            let msg =
              Format.asprintf "Failed to evaluate Component query for system: %s. %a" s.name
                Luma__core.Error.pp e
            in
            log.error (fun l -> l "%s" msg);
            Luma__core.Error.system_run_exn s.name msg)
  else world

let run_list systems world cmd = List.fold_left (fun w sys -> run_system w cmd sys) world systems

(* run 'ready' systems and return pending. *)
let drain world entries cmd =
  let ready, pending =
    List.partition
      (fun { trigger; system = System { run_if; _ } } -> trigger world && run_if world)
      entries
  in
  let world = run_list (List.map (fun e -> e.system) ready) world cmd in
  (world, pending)

let run_stage stage sched world =
  let cmd = Command.create () in
  (* Demote unanchored `At` entries.
     If no "next system" appeared in this stage, move them to the last bucket so they execute at the end
     of this stage, then clear the buffer. *)
  (match Hashtbl.find_opt sched.pending_at stage with
  | Some entries when entries <> [] ->
      let buckets = buckets_or_default sched stage in
      buckets.last <- buckets.last @ entries;
      Hashtbl.replace sched.once stage buckets;
      Hashtbl.replace sched.pending_at stage []
  | _ -> ());

  let systems = Hashtbl.find_opt sched.systems stage |> Option.value ~default:[] |> List.rev in
  let buckets = buckets_or_default sched stage in

  (* Run any 'first' systems that are ready and clear them from the first bucket. *)
  let world, first_pending = drain world buckets.first cmd in
  buckets.first <- first_pending;

  (* Iterate over all systems. Run all 'before' systems before the current system and flush them. 
     Run the current system and repeat with the 'after' systems. *)
  let world =
    List.fold_left
      (fun w (System { uuid; _ } as sys) ->
        let before = Hashtbl.find_opt buckets.before uuid |> Option.value ~default:[] in
        let w, before_pending = drain w before cmd in
        Hashtbl.replace buckets.before uuid before_pending;

        let w = run_list [ sys ] w cmd in

        let after = Hashtbl.find_opt buckets.after uuid |> Option.value ~default:[] in
        let w, after_pending = drain w after cmd in
        Hashtbl.replace buckets.after uuid after_pending;
        w)
      world systems
  in
  (* Run any 'last' systems that are ready and clear them from the 'last' bucket. *)
  let world, last_pending = drain world buckets.last cmd in
  buckets.last <- last_pending;
  Hashtbl.replace sched.once stage buckets;
  Command.flush world cmd;
  world
