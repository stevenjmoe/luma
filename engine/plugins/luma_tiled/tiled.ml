module Make (L : Luma.S) = struct
  module Loader = Loader.Make (L)
  include Types
  open L

  let orientation t = t.orientation
  let render_order t = t.render_order

  let register_map_loader () =
    System.make_with_resources ~components:End
      ~resources:Query.Resource.(Resource (module Asset_server.R) & End)
      "register_map_loader"
      (fun w _ (server, _) ->
        Asset_server.register_loader server
          (module Loader.Tilemap_loader)
          ~ctx_provider:Asset_loader.Context_provider.no_ctx;
        w)

  let register_set_loader () =
    System.make_with_resources ~components:End
      ~resources:Query.Resource.(Resource (module Asset_server.R) & End)
      "register_set_loader"
      (fun w _ (server, _) ->
        Asset_server.register_loader server
          (module Loader.Tileset_loader)
          ~ctx_provider:Asset_loader.Context_provider.no_ctx;
        w)

  let plugin (app : App.t) =
    app |> L.App.on Startup (register_map_loader ()) |> L.App.on Startup (register_set_loader ())
end
