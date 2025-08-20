open Luma__task_queue.Task_queue

type t = {
  assets : Assets.t;
  mutable loaders : Loader.loader_packed list;
}

let create assets_store = { assets = assets_store; loaders = [] }

let register_loader server (loader : Loader.loader_packed) =
  server.loaders <- loader :: server.loaders

let find_loader (type a) (module A : Asset.S with type t = a) server path =
  List.find_opt (fun l -> Loader.match_extension (module A) l ~path) server.loaders

let loader_hooks : (t -> unit) list ref = ref []
let register_loader_hook hook = loader_hooks := hook :: !loader_hooks
let run_loader_hooks server = List.iter (fun hook -> hook server) !loader_hooks

let load (type a) (module A : Asset.S with type t = a) server path =
  let open Luv in
  match find_loader (module A) server path with
  | None -> Error (Luma__core.Error.asset_ext_unsupported path)
  | Some (Packed (module L)) ->
      let handle = Assets.add_pending (module A) server.assets in
      L.begin_load path ~k:(fun r ->
          Complete.push
            (Complete.Pack
               {
                 apply =
                   (fun () ->
                     match r with
                     | Error msg -> Assets.fail server.assets handle msg
                     | Ok d -> (
                         match L.finalize path d with
                         | Ok asset -> Assets.resolve (module A) server.assets handle asset
                         | Error msg -> Assets.fail server.assets handle msg));
               });
          ());
      Ok handle

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "Asset_server"
end)
