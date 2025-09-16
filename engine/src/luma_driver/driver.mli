open Luma__math
open Luma__core

module type S = sig
  type camera
  type colour
  type texture
  type sound
  type music

  val get_frame_time : unit -> float

  module Draw : sig
    val draw_rect : Rect.t -> colour -> unit
    val draw_rect_lines : Rect.t -> float -> colour -> unit
    val draw_circle : int -> int -> float -> colour -> unit
    val draw_text : string -> int -> int -> int -> colour -> unit
  end

  module IO : sig
    type path = string

    val run_io_loop : unit -> unit
    val read_file : path -> k:((bytes, Error.error) result -> unit) -> unit
    val read_file_blocking : path -> string
    val write_file : path -> bytes -> unit
  end

  module Window : sig
    val init : width:int -> height:int -> title:string -> resizable:bool -> unit
    val shutdown : unit -> unit
    val should_close : unit -> bool
    val close : unit -> unit
    val is_fullscreen : unit -> bool
    val is_hidden : unit -> bool
    val is_minimized : unit -> bool
    val is_maximized : unit -> bool
    val is_focused : unit -> bool
    val is_resized : unit -> bool
    val toggle_fullscreen : unit -> unit
    val toggle_borderless_windowed : unit -> unit
    val maximize : unit -> unit
    val minimize : unit -> unit
    val restore : unit -> unit
    val get_frame_time : unit -> float
    val begin_frame : unit -> unit
    val end_frame : unit -> unit
    val begin_2d : camera -> unit
    val end_2d : unit -> unit
    val with_2d : camera -> (unit -> unit) -> unit
    val set_viewport_scissor : int -> int -> int -> int -> unit
    val reset_scissor : unit -> unit
    val clear : colour -> unit
    val screen_width : unit -> int
    val screen_height : unit -> int
    val schedule_next_frame : (unit -> unit) -> unit
  end

  module Camera : sig
    val make :
      offset:Luma__math.Vec2.t ->
      target:Luma__math.Vec2.t ->
      rotation:float ->
      zoom:float ->
      unit ->
      camera

    val default : unit -> camera
    val target : camera -> Vec2.t
    val offset : camera -> Vec2.t
    val zoom : camera -> float
    val rotation : camera -> float
    val set_target : camera -> Vec2.t -> unit
    val set_offset : camera -> Vec2.t -> unit
    val set_zoom : camera -> float -> unit
    val set_rotation : camera -> float -> unit
    val get_world_to_screen_2d : Vec2.t -> camera -> Vec2.t
    val get_screen_to_world_2d : Vec2.t -> camera -> Vec2.t
  end

  module Colour : sig
    val rgb : r:int -> g:int -> b:int -> colour
    val rgba : r:int -> g:int -> b:int -> a:int -> colour
    val white : colour
    val from_string : string -> (colour, Luma__core.Error.error) result
  end

  module Image : sig
    type t

    val load_image : string -> t
    val load_image_from_memory : string -> string -> int -> t
  end

  module Texture : sig
    type t = texture

    val width : t -> int
    val height : t -> int
    val draw_texture : t -> Rect.t -> Rect.t -> Vec2.t -> float -> colour -> unit
    val load_texture_from_image : Image.t -> t
  end

  module Audio : sig
    val init_audio_device : unit -> unit
    val close_audio_device : unit -> unit

    module Music : sig
      type t = music

      val load_music_stream : string -> t
      val is_music_ready : t -> bool
      val unload_music_stream : t -> unit
      val play_music_stream : t -> unit
      val is_music_stream_playing : t -> bool
      val update_music_stream : t -> unit
      val stop_music_stream : t -> unit
      val pause_music_stream : t -> unit
      val resume_music_stream : t -> unit
      val seek_music_stream : t -> float -> unit
      val set_music_volume : t -> float -> unit
      val set_music_pan : t -> float -> unit
      val get_music_time_length : t -> float
      val get_music_time_played : t -> float
    end

    module Sound : sig
      type t = sound

      val load_sound : string -> t
      val play_sound : t -> unit
      val stop_sound : t -> unit
      val pause_sound : t -> unit
      val resume_sound : t -> unit
      val is_sound_playing : t -> bool
      val set_sound_volume : t -> float -> unit
      val set_sound_pan : t -> float -> unit
      val unload_sound : t -> unit
    end
  end

  module Text : sig end

  module Input : sig
    open Luma__types.Input_types

    module Keyboard : sig
      val begin_frame : unit -> unit
      val is_key_down : Key.t -> bool
      val is_key_pressed : Key.t -> bool
      val is_key_released : Key.t -> bool
    end

    module Mouse : sig
      val is_mouse_button_pressed : Mouse_button.t -> bool
      val is_mouse_button_released : Mouse_button.t -> bool
      val is_mouse_button_up : Mouse_button.t -> bool
      val is_mouse_button_down : Mouse_button.t -> bool
      val get_mouse_x : unit -> int
      val get_mouse_y : unit -> int
      val get_mouse_position : unit -> Luma__math.Vec2.t
      val get_mouse_delta : unit -> Luma__math.Vec2.t
      val set_mouse_position : x:int -> y:int -> unit
      val set_mouse_offset : x:int -> y:int -> unit
      val set_mouse_scale : x:float -> y:float -> unit
      val get_mouse_wheel_move : unit -> float
    end
  end

  module UI : sig
    val begin_window :
      title:string -> ?pos:Luma__math.Vec2.t -> ?size:Luma__math.Vec2.t -> unit -> bool

    val end_window : unit -> unit
    val text : string -> unit
  end

  module Debug_draw : sig
    type space =
      [ `World
      | `Screen
      ]

    val line : space -> p0:Luma__math.Vec2.t -> p1:Luma__math.Vec2.t -> colour:colour -> unit
  end
end

include S
