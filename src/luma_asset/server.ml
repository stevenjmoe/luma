type load_error = Unsupported_extension of string | Loader_error of string
type t = { assets : Assets.t; mutable loaders : Loader.t list }

let create assets_store = { assets = assets_store; loaders = [] }
let register_loader server loader = server.loaders <- loader :: server.loaders
let find_loader server path = List.find_opt (fun l -> Loader.match_extension l ~path) server.loaders

let load server path =
  match find_loader server path with
  | None -> Error (Unsupported_extension path)
  | Some loader -> (
      match loader.load path with
      | Error msg -> Error (Loader_error msg)
      | Ok packed ->
          let id = Luma__id.Id.Asset.next () in
          let generation = 1 in
          Assets.add server.assets ~id ~packed ~generation;
          Ok { Assets.id; generation })

module R = Luma__resource.Resource.Make (struct
  type inner = t
end)
