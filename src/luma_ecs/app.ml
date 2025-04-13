type t = {
  world : World.t;
  scheduler : Scheduler.t;
}

let create () = { world = World.create (); scheduler = Scheduler.create () }
let world app = app.world

let add_system sys a =
  Scheduler.add_system a.scheduler sys;
  a

let add_plugin (f : t -> t) app = f app

let register_loaders (server : Luma__asset.Server.t) =
  let image_loader =
    {
      Luma__asset.Loader.exts = [ ".png"; ".jpg" ];
      load =
        (fun path ->
          let image = Raylib.load_image path in
          let texture = Raylib.load_texture_from_image image in
          Ok (Loaded ((module Luma__image.Image.Texture.A), texture)));
    }
  in
  Luma__asset.Server.register_loader server image_loader

let update_time =
  System.make
    ~components:Query.(End)
    (fun (world : World.t) _ ->
      match World.get_resource world Resources.Time.R.id with
      | Some r -> (
          match Luma__resource.Resource.unpack (module Resources.Time.R) r with
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

  let time = Resources.Time.{ dt = 0.0016; elapsed = 0. } in
  let packed_time = Luma__resource.Resource.pack (module Resources.Time.R) time in

  let assets = Luma__asset.Assets.create () in
  let packed_assets = Luma__resource.Resource.pack (module Luma__asset.Assets.R) assets in

  let asset_server = Luma__asset.Server.create assets in
  register_loaders asset_server;
  let packed_asset_server =
    Luma__resource.Resource.pack (module Luma__asset.Server.R) asset_server
  in

  let world =
    a.world
    |> World.add_resource Resources.Time.R.id packed_time
    |> World.add_resource Luma__asset.Assets.R.id packed_assets
    |> World.add_resource Luma__asset.Server.R.id packed_asset_server
  in

  world |> Scheduler.run_startup_systems a.scheduler |> ignore;
  add_system (Scheduler.Update (System.WithoutResources update_time)) a |> ignore;

  let camera =
    try
      World.query world Query.(Required (module Components.Camera.C) & End)
      |> List.map (fun (_, (camera, _)) -> camera)
      |> List.hd
    with _ ->
      failwith
        "Luma expects at least 1 camera added to the world as a component. Only the first one will \
         be used."
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
