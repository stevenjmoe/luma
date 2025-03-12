include Resources
include Components

type t = { world : World.t; scheduler : Scheduler.t; plugins : (World.t -> unit) list }

let create () = { world = World.create (); scheduler = Scheduler.create (); plugins = [] }

let add_system sys a =
  Scheduler.add_system a.scheduler sys;
  a

let add_plugin p a = { a with plugins = p :: a.plugins }

let update_time =
  System.make
    Query.(End)
    (fun (world : World.t) _ ->
      match World.get_resource world Time.R.id with
      | Some r -> (
          match Resource.unpack (module Time.R) r with
          | Some time ->
              let dt = Raylib.get_frame_time () in
              time.dt <- dt;
              time.elapsed <- time.elapsed +. dt;
              world
          | None -> world)
      | None -> world)

let run a =
  Raylib.init_window 800 450 "";
  Raylib.set_target_fps 60;

  let textures = Texture.create () in
  let packed_texture = Resource.pack (module Texture.R) textures in

  let texture_atlases = Texture_atlas.create () in
  let packed_atlases = Resource.pack (module Texture_atlas.R) texture_atlases in

  let time = Time.{ dt = 0.0016; elapsed = 0. } in
  let packed_time = Resource.pack (module Time.R) time in

  let world =
    a.world
    |> World.add_resource Time.R.id packed_time
    |> World.add_resource Texture.R.id packed_texture
    |> World.add_resource Texture_atlas.R.id packed_atlases
  in

  let world =
    List.fold_right
      (fun plugin world ->
        plugin world;
        world)
      a.plugins world
  in

  world |> Scheduler.run_startup_systems a.scheduler |> ignore;
  add_system (Scheduler.Update (System.WithoutResources update_time)) a |> ignore;

  let camera =
    World.query world Query.(Required (module Camera.C) & End)
    |> List.map (fun (_, (camera, _)) -> camera)
    |> List.hd
  in

  let rec loop (world, scheduler) =
    if Raylib.window_should_close () then
      Raylib.close_window ()
    else
      let open Raylib in
      begin_drawing ();
      begin_mode_2d camera;
      Raylib.draw_rectangle 10 10 250 113 Raylib.Color.black;
      let world = Scheduler.run_update_systems a.scheduler a.world in
      clear_background Color.beige;
      end_mode_2d ();
      end_drawing ();
      loop (world, a.scheduler)
  in
  loop (world, a.scheduler)
