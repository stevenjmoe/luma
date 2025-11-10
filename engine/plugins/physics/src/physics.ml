open Luma__core
open Luma__math
open Luma__storage

module Make (L : Luma.S) = struct
  module S = Systems.Make (L)

  let get_pos world entity =
    let ( let* ) = Option.bind in
    let* store_packed = L.World.get_resource world Rb_store.R.type_id in
    let* store = L.Resource.unpack_opt (module Rb_store.R) store_packed in

    let* index_packed = L.World.get_resource world Rb_store.Index.R.type_id in
    let* index = L.Resource.unpack_opt (module Rb_store.Index.R) index_packed in

    match Rb_store.Index.row_of_entity index (L.Id.Entity.to_int entity) with
    | Some row -> Some (Vec2.create store.pos_x.(row) store.pos_y.(row))
    | None -> None

  let setup app world_config =
    let config = match world_config with None -> Config.default () | Some c -> c in
    let world = L.App.world app in

    let packed = L.Resource.pack (module Config.R) config in
    L.World.add_resource Config.R.type_id packed world |> ignore;

    let body_store = Rb_store.create ~initial:50 () in
    let packed_store = L.Resource.pack (module Rb_store.R) body_store in
    L.World.add_resource Rb_store.R.type_id packed_store world |> ignore;

    let rb_index = Rb_store.Index.create ~initial:50 in
    let packed_index = L.Resource.pack (module Rb_store.Index.R) rb_index in
    L.World.add_resource Rb_store.Index.R.type_id packed_index world |> ignore;

    let grid = Grid.create config.bounds 2. in
    let packed_grid = L.Resource.pack (module Grid.R) grid in
    L.World.add_resource Grid.R.type_id packed_grid world |> ignore;

    let broad_phase = Broad_phase.create () in
    let packed_bp = L.Resource.pack (module Broad_phase.R) broad_phase in
    L.World.add_resource Broad_phase.R.type_id packed_bp world |> ignore;

    let narrow_phase = Narrow_phase.create () in
    let packed_np = L.Resource.pack (module Narrow_phase.R) narrow_phase in
    L.World.add_resource Narrow_phase.R.type_id packed_np world |> ignore;

    app

  let plugin ?(world_config = None) app =
    let app = setup app world_config in

    app |> L.App.on PreUpdate (S.sync_rigid_bodies ()) |> L.App.on Update (S.step ())
end
