open Luma__driver
open Luma__ecs
open Luma__app

module type Window_config = sig
  type colour

  type t = {
    height : int;
    width : int;
    colour : colour option;
    title : string option;
  }

  val default : unit -> t

  val create : int -> int -> colour option -> string option -> t
  (** [create width height colour title] *)
end

module type S = sig
  type colour

  module Window_config : Window_config with type colour = colour

  val plugin : ?config:Window_config.t -> App.t -> App.t
end

module Make (D : Driver.S) : S with type colour = D.colour = struct
  type colour = D.colour

  module Window_config : Window_config with type colour = D.colour = struct
    type colour = D.colour

    type t = {
      height : int;
      width : int;
      colour : colour option;
      title : string option;
    }

    let default () = { width = 1920; height = 1080; colour = Some D.Colour.white; title = None }

    let create width height colour title =
      let colour = Option.value colour ~default:D.Colour.white in
      let title = Option.value title ~default:"" in
      { width; height; colour = Some colour; title = Some title }
  end

  let clear_window (config : Window_config.t) =
    System.make ~components:End "clear_window" (fun world _ ->
        let colour = Option.value config.colour ~default:D.Colour.white in
        D.Window.clear colour;
        world)

  let init (config : Window_config.t) =
    System.make ~components:End "init" (fun world _ ->
        D.Window.init ~width:config.width ~height:config.height
          ~title:(Option.value config.title ~default:"");
        world)

  let plugin ?(config : Window_config.t = Window_config.default ()) app =
    app |> App.on PreUpdate (clear_window config) |> App.on PreStartup (init config)
end
