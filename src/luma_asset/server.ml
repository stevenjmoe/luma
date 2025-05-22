type load_error =
  | Unsupported_extension of string
  | Loader_error of string

type t = {
  assets : Assets.t;
  mutable loaders : Loader.t list;
}

let create assets_store = { assets = assets_store; loaders = [] }
let register_loader server loader = server.loaders <- loader :: server.loaders

let find_loader (type a) (module A : Asset.S with type t = a) server path =
  List.find_opt (fun l -> Loader.match_extension (module A) l ~path) server.loaders

let loader_hooks : (t -> unit) list ref = ref []
let register_loader_hook hook = loader_hooks := hook :: !loader_hooks
let run_loader_hooks server = List.iter (fun hook -> hook server) !loader_hooks

let load (type a) (module A : Asset.S with type t = a) server path =
  match find_loader (module A) server path with
  | None -> Error (Unsupported_extension path)
  | Some loader -> (
      match loader.load path with
      | Error msg -> Error (Loader_error msg)
      | Ok (Loaded (asset_mod, asset)) ->
          let handle = Assets.add asset_mod server.assets asset in
          Ok handle)

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "Asset_server"
end)
