open Luma__driver
open Luma__ecs
open Luma__app
open Luma__resource

module type Window_config = sig
  type colour

  type t = {
    height : int;
    width : int;
    mutable colour : colour option;
    title : string option;
    resizable : bool;
  }

  module R : Resource.S with type t = t

  val default : unit -> t

  val create : int -> int -> colour option -> string option -> bool -> t
  (** [create width height colour title resizeable] *)
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
      mutable colour : colour option;
      title : string option;
      resizable : bool;
    }

    module R = Luma__resource.Resource.Make (struct
      type inner = t

      let name = "window_config_res"
    end)

    let default () =
      { width = 1920; height = 1080; colour = Some D.Colour.white; title = None; resizable = false }

    let create width height colour title resizable =
      let colour = Option.value colour ~default:D.Colour.white in
      let title = Option.value title ~default:"" in
      { width; height; colour = Some colour; title = Some title; resizable }
  end

  let clear_window () =
    System.make_with_resources ~components:End
      ~resources:Query.Resource.(Resource (module Window_config.R) & End)
      "clear_window"
      (fun world _ _ (config, _) ->
        let colour = Option.value config.colour ~default:D.Colour.white in
        D.Window.clear colour;
        world)

  let init (config : Window_config.t) =
    System.make ~components:End "init" (fun world _ _ ->
        let packed = Resource.pack (module Window_config.R) config in
        let world = World.add_resource Window_config.R.type_id packed world in
        D.Window.init ~width:config.width ~height:config.height
          ~title:(Option.value config.title ~default:"")
          ~resizable:config.resizable;
        world)

  let plugin ?(config : Window_config.t = Window_config.default ()) app =
    app |> App.on PreUpdate (clear_window ()) |> App.on PreStartup (init config)
end
