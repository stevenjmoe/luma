module type S = sig
  val begin_window :
    title:string -> ?pos:Luma__math.Vec2.t -> ?size:Luma__math.Vec2.t -> unit -> bool

  val end_window : unit -> unit
  val text : string -> unit
end

module Make (D : Luma__driver.Driver.S) : S = struct
  let begin_window = D.UI.begin_window
  let end_window = D.UI.end_window
  let text = D.UI.text
end
