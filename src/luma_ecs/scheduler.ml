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

type system = System : (World.t, 'a) System.t -> system
type t = { systems : (stage, system list) Hashtbl.t }

let create () =
  let systems = Hashtbl.create 16 in
  List.iter
    (fun stage -> Hashtbl.add systems stage [])
    [
      PreStartup; Startup; PostStartup; PreUpdate; Update; PostUpdate; PreRender; Render; PostRender;
    ];
  { systems }

let add_system sched stage sys =
  let systems = Hashtbl.find sched.systems stage in
  Hashtbl.replace sched.systems stage (System sys :: systems)

let add_scheduled (sched : t) (s : scheduled) =
  match s with
  | PreStartup s -> add_system sched PreStartup s
  | Startup s -> add_system sched Startup s
  | PostStartup s -> add_system sched PostStartup s
  | PreUpdate s -> add_system sched PreUpdate s
  | Update s -> add_system sched Update s
  | PostUpdate s -> add_system sched PostUpdate s
  | PreRender s -> add_system sched PreRender s
  | Render s -> add_system sched Render s
  | PostRender s -> add_system sched PostRender s

let run_system (world : World.t) (system : (World.t, 'a) System.t) : World.t =
  let archetypes = World.archetypes world |> Hashtbl.to_seq_values |> List.of_seq in
  match system with
  | System.WithoutResources s ->
      let matching_entities = Query.evaluate ~filter:s.filter s.components_query archetypes in
      s.run world matching_entities
  | System.WithResources s -> (
      let matching_entities = Query.evaluate ~filter:s.filter s.components_query archetypes in
      match Luma__resource.Resource.Query.evaluate s.resources_query (World.resources world) with
      | Ok resource_value -> s.run world matching_entities resource_value
      | Error e ->
          failwith
            (Printf.sprintf "Failed to run system. %s" (Luma__resource.Resource.error_to_string e)))

let run_stage stage sched world =
  Hashtbl.find_opt sched.systems stage
  |> Option.value ~default:[]
  |> List.rev
  |> List.fold_left (fun w (System sys) -> run_system world sys) world
