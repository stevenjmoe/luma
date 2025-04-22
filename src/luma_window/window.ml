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
end

module Make (D : Driver.S) = struct
  module Window_config : Window_config with type colour = D.colour = struct
    type colour = D.colour

    type t = {
      height : int;
      width : int;
      colour : colour option;
      title : string option;
    }

    let default () = { height = 1080; width = 1920; colour = Some D.Colour.white; title = None }

    let create height width colour title =
      let colour = Option.value colour ~default:D.Colour.white in
      let title = Option.value title ~default:"" in
      { height; width; colour = Some colour; title = Some title }
  end

  let clear_window (config : Window_config.t) =
    System.make ~components:End (fun world entities ->
        let colour = Option.value config.colour ~default:D.Colour.white in
        D.Window.clear colour;
        world)

  let init (config : Window_config.t) =
    System.make ~components:End (fun world entities ->
        D.Window.init ~width:config.width ~height:config.height
          ~title:(Option.value config.title ~default:"");
        world)

  let plugin ?(config : Window_config.t = Window_config.default ()) app =
    app
    |> App.add_system (PreUpdate (WithoutResources (clear_window config)))
    |> App.add_system (PreStartup (WithoutResources (init config)))
end
