include Resources
include Components
module Assets = Luma__asset.Assets
module Asset_server = Luma__asset.Server

type t = { world : World.t; scheduler : Scheduler.t; plugins : (World.t -> unit) list }

let create () = { world = World.create (); scheduler = Scheduler.create (); plugins = [] }

let add_system sys a =
  Scheduler.add_system a.scheduler sys;
  a

let add_plugin p a = { a with plugins = p :: a.plugins }

let register_loaders (server : Asset_server.t) =
  let image_loader =
    {
      Luma__asset.Loader.exts = [ ".png" ];
      load =
        (fun path ->
          let image = Raylib.load_image path in
          let texture = Raylib.load_texture_from_image image in
          Ok (Asset.pack (module Luma__asset.Assets.Texture.A) texture));
    }
  in
  Asset_server.register_loader server image_loader

let update_time =
  System.make
    ~components:Query.(End)
    (fun (world : World.t) _ ->
      match World.get_resource world Time.R.id with
      | Some r -> (
          match Resource.unpack (module Time.R) r with
          | Ok time ->
              let dt = Raylib.get_frame_time () in
              time.dt <- dt;
              time.elapsed <- time.elapsed +. dt;
              world
          | Error _ -> failwith "could not get time from resources")
      | None -> failwith "could not get time from resources")

let run a =
  Raylib.init_window 1800 800 "";
  Raylib.set_target_fps 60;

  let time = Time.{ dt = 0.0016; elapsed = 0. } in
  let packed_time = Resource.pack (module Time.R) time in

  let assets = Assets.create () in
  let packed_assets = Resource.pack (module Assets.R) assets in

  let asset_server = Asset_server.create assets in
  register_loaders asset_server;
  let packed_asset_server = Resource.pack (module Asset_server.R) asset_server in

  let world =
    a.world
    |> World.add_resource Time.R.id packed_time
    |> World.add_resource Assets.R.id packed_assets
    |> World.add_resource Asset_server.R.id packed_asset_server
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
      Lwt_main.run (Lwt.pause ());
      begin_drawing ();
      begin_mode_2d camera;
      let world = Scheduler.run_update_systems a.scheduler a.world in
      clear_background Color.beige;
      end_mode_2d ();
      end_drawing ();
      loop (world, a.scheduler)
  in
  loop (world, a.scheduler)
