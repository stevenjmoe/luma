open Luma__math

module type S = sig
  type camera
  type colour
  type texture
  type sound
  type music

  val draw_rect : Rect.t -> colour -> unit
  val get_frame_time : unit -> float

  module Window : sig
    val init : width:int -> height:int -> title:string -> unit
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
    val clear : colour -> unit
    val screen_width : unit -> int
    val screen_height : unit -> int
  end

  module Camera : sig
    val make :
      position:Luma__math.Vec2.t ->
      target:Luma__math.Vec2.t ->
      rotation:float ->
      zoom:float ->
      camera

    val default : unit -> camera
    val set_target : camera -> float * float -> unit
  end

  module Colour : sig
    val rgb : r:int -> g:int -> b:int -> colour
    val rgba : r:int -> g:int -> b:int -> a:int -> colour
    val white : colour
  end

  module Image : sig
    type t

    val load_image : string -> t
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
end

include S
