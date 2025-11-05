open Luma__core
open Luma__math
open Luma__storage

module Make (L : Luma.S) = struct
  module S = Systems.Make (L)

  let get_pos world entity =
    let ( let* ) = Option.bind in
    let* store_packed = L.World.get_resource world Pb_store.R.type_id in
    let* store = L.Resource.unpack_opt (module Pb_store.R) store_packed in

    let* index_packed = L.World.get_resource world Pb_store.Index.R.type_id in
    let* index = L.Resource.unpack_opt (module Pb_store.Index.R) index_packed in

    match Pb_store.Index.row_of_entity index (L.Id.Entity.to_int entity) with
    | Some row -> Some (Vec2.create store.pos_x.(row) store.pos_y.(row))
    | None -> None

  let setup app world_config =
    let config = match world_config with None -> Config.default () | Some c -> c in
    let world = L.App.world app in

    let packed = L.Resource.pack (module Config.R) config in
    L.World.add_resource Config.R.type_id packed world |> ignore;

    let body_store = Pb_store.create ~initial:50 () in
    let packed_store = L.Resource.pack (module Pb_store.R) body_store in
    L.World.add_resource Pb_store.R.type_id packed_store world |> ignore;

    let rb_index = Pb_store.Index.create ~initial:50 in
    let packed_index = L.Resource.pack (module Pb_store.Index.R) rb_index in
    L.World.add_resource Pb_store.Index.R.type_id packed_index world |> ignore;

    let grid = Grid.create config.bounds 20. in
    let packed_grid = L.Resource.pack (module Grid.R) grid in
    L.World.add_resource Grid.R.type_id packed_grid world |> ignore;

    app

  let plugin ?(world_config = None) app =
    let app = setup app world_config in

    app |> L.App.on PreUpdate (S.sync_rigid_bodies ()) |> L.App.on Update (S.step ())
end
