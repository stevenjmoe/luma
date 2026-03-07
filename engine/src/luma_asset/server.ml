open Luma__core

type pending =
  | Pending : {
      id : int;
      path : string;
      handle : Assets.handle;
      loader : Loader.loader_packed;
    }
      -> pending

type t = {
  assets : Assets.t;
  mutable loaders : Loader.loader_packed list;
  path_to_handle : (string, Assets.handle) Hashtbl.t;
  pending_by_id : (int, pending) Hashtbl.t;
}

let create assets_store =
  let path_to_handle = Hashtbl.create 16 in
  let pending_by_id = Hashtbl.create 64 in
  { assets = assets_store; loaders = []; path_to_handle; pending_by_id }

let register_loader
    (type c t)
    server
    (module L : Loader.LOADER with type t = t and type ctx = c)
    ~ctx_provider =
  let loader = Loader.Packed { loader = (module L); ctx_provider } in
  server.loaders <- loader :: server.loaders

let find_loader (type a) (module A : Asset.S with type t = a) server path =
  List.find_opt (fun l -> Loader.match_extension (module A) l ~path) server.loaders

let loader_hooks : (t -> unit) list ref = ref []
let register_loader_hook hook = loader_hooks := hook :: !loader_hooks
let run_loader_hooks server = List.iter (fun hook -> hook server) !loader_hooks
let log = Log.sub_log "asset_server"

let load_inner (type a) (module A : Asset.S with type t = a) server path =
  match find_loader (module A) server path with
  | None -> Error (Luma__core.Error.asset_ext_unsupported path)
  | Some packed_laoder ->
      let handle = Assets.add_pending (module A) server.assets ~path in
      Hashtbl.replace server.path_to_handle path handle;

      let id =
        match packed_laoder with
        | Packed { loader = (module L); ctx_provider = _cp } -> L.begin_load path
      in
      Hashtbl.replace server.pending_by_id id (Pending { id; path; handle; loader = packed_laoder });
      Ok handle

(* If an asset with this path has already begun loading, return its handle. 
   If the asset is registered with the server, but not with the Assets registry,
   re-load and replace the path/handle in the server registry. *)
let load (type a) (module A : Asset.S with type t = a) server path =
  match Hashtbl.find_opt server.path_to_handle path with
  | None -> (
      match load_inner (module A) server path with Ok handle -> Ok handle | Error e -> Error e)
  | Some handle -> (
      if Assets.exists server.assets handle then Ok handle
      else
        match load_inner (module A) server path with Ok handle -> Ok handle | Error e -> Error e)

let load_exn (type a) (module A : Asset.S with type t = a) server path =
  load (module A) server path |> Result.get_ok

let apply_io_events server world (events : 'io_event list) =
  events
  |> List.iter (function
    | Io.Event.File_read_ok { id; bytes; path = _ } -> (
        match Hashtbl.find_opt server.pending_by_id id with
        | None -> ()
        | Some (Pending { path; handle; loader; _ }) -> (
            Hashtbl.remove server.pending_by_id id;

            (* finalize via loader + ctx_provider *)
            match loader with
            | Loader.Packed { loader = (module Loader); ctx_provider } -> (
                let ctx_res =
                  match ctx_provider with `Static c -> Ok c | `From_world f -> f world
                in
                match ctx_res with
                | Error e ->
                    log.error (fun l -> l "Failed to load asset: %a" Error.pp e);
                    Assets.fail server.assets handle { path; msg = "Invalid context provider." }
                | Ok ctx -> (
                    match Loader.finalize ctx path bytes with
                    | Ok packed -> Assets.resolve server.assets handle packed
                    | Error e ->
                        log.error (fun l -> l "Failed to finalize asset: %a" Error.pp e);
                        Assets.fail server.assets handle
                          { path; msg = "Failed to finalize asset load." }))))
    | File_read_err { id; err; _ } -> (
        match Hashtbl.find_opt server.pending_by_id id with
        | None -> ()
        | Some (Pending { path; handle; _ }) ->
            Hashtbl.remove server.pending_by_id id;
            log.error (fun l -> l "Failed to load asset: %a" Error.pp err);
            let msg = Format.asprintf "e:  %a" Error.pp err in

            Assets.fail server.assets handle
              { path; msg = Printf.sprintf "Failed to load asset: %s" msg }))

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "Asset_server"
end)
