module Make (L : Luma.S) : sig
  val plugin : L.App.t -> L.App.t
end
