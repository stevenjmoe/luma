type t = {
  world : World.t;
  scheduler : Scheduler.t;
  plugins : (t -> t) list;
}

let create () = { world = World.create (); scheduler = Scheduler.create (); plugins = [] }
let world (app : t) = app.world
let scheduler (app : t) = app.scheduler
let add_plugin plugin app = { app with plugins = plugin :: app.plugins }

let add_system sys app =
  Scheduler.add_scheduled app.scheduler sys;
  app

(* TODO: this shouldn't reference raylib *)
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

  let world = Scheduler.run_stage Scheduler.Startup app.scheduler app.world in
  let app = List.fold_left (fun app plugin -> plugin app) app app.plugins in
  let app = { app with world } in

  let rec loop (world, scheduler) =
    if D.should_close () then
      D.shutdown ()
    else (
      D.begin_frame ();
      D.clear Raylib.Color.beige;
      Scheduler.run_stage PreUpdate scheduler world |> ignore;
      Scheduler.run_stage Update scheduler world |> ignore;
      Scheduler.run_stage PostUpdate scheduler world |> ignore;

      Scheduler.run_stage PreRender scheduler world |> ignore;
      Scheduler.run_stage Render scheduler world |> ignore;
      Scheduler.run_stage PostRender scheduler world |> ignore;

      D.end_frame ();
      loop (world, scheduler))
  in
  loop (app.world, app.scheduler)

let run app = run_with_driver (module Raylib_driver) app
