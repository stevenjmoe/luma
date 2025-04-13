let plugin app =
  let open Luma__ecs in
  let world = App.world app in
  let assets = Assets.create () in
  let packed_assets = Luma__resource.Resource.pack (module Assets.R) assets in
  let asset_server = Server.create assets in
  let packed_asset_server = Luma__resource.Resource.pack (module Server.R) asset_server in

  Server.run_loader_hooks asset_server;

  world
  |> World.add_resource Assets.R.id packed_assets
  |> World.add_resource Server.R.id packed_asset_server
  |> ignore;
  app
