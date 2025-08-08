open Raylib

let init ~width ~height ~title =
  set_trace_log_level TraceLogLevel.Warning;
  set_config_flags [ ConfigFlags.Window_resizable ];
  init_window width height title;
  set_target_fps 60

let shutdown = close_window
let should_close = window_should_close
let close = close_window
let is_fullscreen = is_window_fullscreen
let is_hidden = is_window_hidden
let is_minimized = is_window_minimized
let is_maximized = is_window_maximized
let is_focused = is_window_focused
let is_resized = is_window_resized
let toggle_fullscreen = toggle_fullscreen
let toggle_borderless_windowed = toggle_borderless_windowed
let maximize = maximize_window
let minimize = minimize_window
let restore = restore_window
let get_frame_time = get_frame_time
let begin_frame = begin_drawing
let end_frame = end_drawing
let begin_2d = begin_mode_2d
let end_2d = end_mode_2d
let clear = clear_background
let screen_width = get_screen_width
let screen_height = get_screen_height
let schedule_next_frame f = f ()
