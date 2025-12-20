module type S = sig
  type music
  type t

  type state =
    [ `Paused
    | `Playing
    | `Stopped
    ]

  module A : Luma__asset.Asset.S with type t = t

  val play : ?volume:float -> ?loop:bool -> t -> unit
  val pause : t -> unit
  val resume : t -> unit
  val stop : t -> unit
  val volume : t -> float
  val set_volume : t -> float -> unit
  val pan : t -> float
  val set_pan : t -> float -> unit
  val is_playing : t -> bool
  val progress : t -> float option
  val state : t -> state
  val plugin : Luma__app__App.t -> Luma__app__App.t
end

module Make : functor (D : Luma__driver.Driver.S) -> S with type music = D.Audio.Music.t
