open Luma__asset
open Luma__app
open Luma__resource
open Luma__ecs

let asset_plugin app =
  let world = App.world app in
  let assets = Assets.create () in
  let packed_assets = Resource.pack (module Assets.R) assets in
  let asset_server = Server.create assets in
  let packed_asset_server = Resource.pack (module Server.R) asset_server in

  Server.run_loader_hooks asset_server;

  world
  |> World.add_resource Assets.R.type_id packed_assets
  |> World.add_resource Server.R.type_id packed_asset_server
  |> ignore;
  app
