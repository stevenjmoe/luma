open Luma__asset
open Luma__math

type load_state =
  | Waiting_source
  | Loading_tilesets of { tileset_handles_by_gid : (int * Assets.handle) list }
  | Loaded of Map.t
  | Load_failed of Luma__core.Error.error

type render_state =
  | Render_not_started
  | Loading_textures of { textures_by_tileset : (int, Types.tileset_texture) Hashtbl.t }
  | Renderable of {
      tilesets : (int, Types.tileset_loaded) Hashtbl.t;
      plan : Plan.t;
    }
  | Render_failed of Luma__core.Error.error

type entry = {
  origin : Vec2.t;
  scale : float;
  layers : string list option;
  z_base : int;
  mutable load_state : load_state;
  mutable render_state : render_state;
  mutable collision_extracted : bool;
}

type t = (Assets.handle, entry) Hashtbl.t

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "tiled_maps"
end)

let create_entry ~origin ~scale ~z_base =
  {
    origin;
    scale;
    layers = None;
    z_base;
    load_state = Waiting_source;
    render_state = Render_not_started;
    collision_extracted = false;
  }

let map entry = match entry.load_state with Loaded map -> Some map | _ -> None
let loaded entry = Option.is_some (map entry)

let render_plan entry =
  match entry.render_state with Renderable { plan; _ } -> Some plan | _ -> None

let renderable entry = Option.is_some (render_plan entry)
