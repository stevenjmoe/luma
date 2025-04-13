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

module type Driver = sig
  val init : unit -> unit
  val shutdown : unit -> unit
  val should_close : unit -> bool
  val get_frame_time : unit -> float
  val begin_frame : unit -> unit
  val end_frame : unit -> unit
  val begin_2d : Raylib.Camera2D.t -> unit
  val end_2d : unit -> unit
  val clear : Raylib.Color.t -> unit
end

module Raylib_driver : Driver = struct
  let init () =
    Raylib.init_window 1800 800 "";
    Raylib.set_target_fps 60

  let shutdown () = Raylib.close_window ()
  let should_close () = Raylib.window_should_close ()
  let get_frame_time () = Raylib.get_frame_time ()
  let begin_frame () = Raylib.begin_drawing ()
  let end_frame () = Raylib.end_drawing ()
  let begin_2d = Raylib.begin_mode_2d
  let end_2d () = Raylib.end_mode_2d ()
  let clear = Raylib.clear_background
end

let run_with_driver (type d) (module D : Driver) (app : t) =
  D.init ();

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
    app.world
    |> World.add_resource Resources.Time.R.id packed_time
    |> World.add_resource Luma__asset.Assets.R.id packed_assets
    |> World.add_resource Luma__asset.Server.R.id packed_asset_server
  in

  let world = Scheduler.run_startup_systems app.scheduler world in
  let app = { app with world } in
  let app = add_system (Scheduler.Update (System.WithoutResources update_time)) app in

  let rec loop (world, scheduler) =
    if D.should_close () then
      D.shutdown ()
    else (
      Lwt_main.run (Lwt.pause ());
      D.begin_frame ();

      let world = Scheduler.run_update_systems scheduler world in

      D.clear Raylib.Color.beige;
      D.end_frame ();
      loop (world, scheduler))
  in
  loop (app.world, app.scheduler)

let run app = run_with_driver (module Raylib_driver) app
