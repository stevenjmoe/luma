module Resource = Luma__resource.Resource

type schedule =
  | Startup : (World.t, 'a) System.t -> schedule
  | Update : (World.t, 'a) System.t -> schedule

type t = { mutable startup_systems : schedule list; mutable update_systems : schedule list }

let create () = { startup_systems = []; update_systems = [] }

let add_system (sched : t) (sys : schedule) =
  match sys with
  | Startup s -> sched.startup_systems <- Startup s :: sched.startup_systems
  | Update s -> sched.update_systems <- Update s :: sched.update_systems

let run_system (world : World.t) (system : (World.t, 'a) System.t) : World.t =
  let archetypes = World.archetypes world |> Hashtbl.to_seq_values |> List.of_seq in
  match system with
  | System.WithoutResources s ->
      let matching_entities = Query.evaluate ~filter:s.filter s.components_query archetypes in
      s.run world matching_entities
  | System.WithResources s -> (
      let matching_entities = Query.evaluate ~filter:s.filter s.components_query archetypes in
      match Resource.Query.evaluate s.resources_query (World.resources world) with
      | Ok resource_value -> s.run world matching_entities resource_value
      | Error e ->
          failwith
            (Printf.sprintf "Failed to run system. %s"
               (Luma__tracked_module.Tracked_module.error_to_string e)))

let run_startup_systems (sched : t) (world : World.t) : World.t =
  let world' =
    List.fold_left
      (fun w s -> match s with Startup sys -> run_system w sys | Update _ -> w)
      world (List.rev sched.startup_systems)
  in
  sched.startup_systems <- [];
  world'

let run_update_systems (sched : t) (world : 'w) : 'w =
  List.fold_left
    (fun w s -> match s with Update sys -> run_system w sys | Startup _ -> w)
    world (List.rev sched.update_systems)

(*let run_all (sched : 'w t) (world : 'w) : 'w =
  let world = run_startup_systems sched world in
  run_update_systems sched world*)
