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

let stage_name = function
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
      sys : (World.t, _) System.t;
      run_if : World.t -> bool;
    }
      -> system

type t = { systems : (stage, system list) Hashtbl.t }

let create () =
  log.info (fun log -> log "Creating scheduler.");
  let systems = Hashtbl.create 16 in
  List.iter
    (fun stage -> Hashtbl.add systems stage [])
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
  { systems }

let add_system (sched : t) (stage : stage) (system : system) =
  let systems = Hashtbl.find sched.systems stage in
  Hashtbl.replace sched.systems stage (system :: systems)

let run_system (world : World.t) (System { sys = system; run_if }) : World.t =
  if run_if world then
    let archetypes = World.archetypes world |> Hashtbl.to_seq_values |> List.of_seq in
    match system with
    | System.WithoutResources s -> (
        match Query.Component.evaluate ~filter:s.filter s.components_query archetypes with
        | Ok matching_entities -> s.run world matching_entities
        | Error e ->
            let msg = Format.asprintf "Failed to run system: %s. %a" s.name Luma__core.Error.pp e in
            log.error (fun l -> l "%s" msg);
            Luma__core.Error.system_run_exn s.name msg)
    | System.WithResources s -> (
        match Query.Component.evaluate ~filter:s.filter s.components_query archetypes with
        | Ok matching_entities -> (
            match Query.Resource.evaluate s.resources_query (World.resources world) with
            | Ok resource_value -> s.run world matching_entities resource_value
            | Error e ->
                let msg =
                  Format.asprintf "Failed evaluate Resource query for system: %s. %a" s.name
                    Luma__core.Error.pp e
                in
                log.error (fun l -> l "%s" msg);
                Luma__core.Error.system_run_exn s.name msg)
        | Error e ->
            let msg =
              Format.asprintf "Failed evaluate Component query for system: %s. %a" s.name
                Luma__core.Error.pp e
            in
            log.error (fun l -> l "%s" msg);
            Luma__core.Error.system_run_exn s.name msg)
  else world

let run_stage stage sched world =
  Hashtbl.find_opt sched.systems stage
  |> Option.value ~default:[]
  |> List.rev
  |> List.fold_left (fun w sys -> run_system w sys) world
