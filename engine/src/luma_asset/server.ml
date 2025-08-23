open Luma__task_queue.Task_queue
open Luma__core

type t = {
  assets : Assets.t;
  mutable loaders : Loader.loader_packed list;
}

let create assets_store = { assets = assets_store; loaders = [] }

let register_loader
    (type c t)
    server
    (module L : Loader.LOADER with type t = t and type ctx = c)
    ~ctx_provider =
  let loader = Loader.Packed { l = (module L); cp = ctx_provider } in
  server.loaders <- loader :: server.loaders

let find_loader (type a) (module A : Asset.S with type t = a) server path =
  List.find_opt (fun l -> Loader.match_extension (module A) l ~path) server.loaders

let loader_hooks : (t -> unit) list ref = ref []
let register_loader_hook hook = loader_hooks := hook :: !loader_hooks
let run_loader_hooks server = List.iter (fun hook -> hook server) !loader_hooks
let log = Log.sub_log "asset_server"

let load (type a) (module A : Asset.S with type t = a) server path world =
  let open Luv in
  match find_loader (module A) server path with
  | None -> Error (Luma__core.Error.asset_ext_unsupported path)
  | Some (Packed { l = (module L); cp }) ->
      let handle = Assets.add_pending (module A) server.assets in
      L.begin_load path ~k:(fun r ->
          Complete.push
            (Complete.Pack
               {
                 apply =
                   (fun () ->
                     match r with
                     | Error e ->
                         log.error (fun l -> l "Failed to load asset: %a" Luma__core.Error.pp e);
                         Assets.fail server.assets handle { path; msg = "Failed to load asset." }
                     | Ok d -> (
                         let ctx_res =
                           match cp with `Static c -> Ok c | `From_world f -> f world
                         in
                         match ctx_res with
                         | Error e ->
                             log.error (fun l -> l "Failed to load asset: %a" Luma__core.Error.pp e);
                             Assets.fail server.assets handle
                               { path; msg = "Invalid context provider." }
                         | Ok ctx -> (
                             match L.finalize ctx path d with
                             | Ok asset -> Assets.resolve (module A) server.assets handle asset
                             | Error e ->
                                 log.error (fun l ->
                                     l "Failed to load asset: %a" Luma__core.Error.pp e);
                                 Assets.fail server.assets handle
                                   { path; msg = "Failed to finalize asset load." })));
               }));
      Ok handle

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "Asset_server"
end)
