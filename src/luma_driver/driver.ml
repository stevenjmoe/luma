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
