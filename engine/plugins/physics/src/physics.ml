open Luma__core
open Luma__math
open Luma__storage

module Make (L : Luma.S) = struct
  module Config = Config.Make (L)
  module Rigid_body = Rigid_body.Make (L) (Config)
  module Store = Storage.Make (L)
  module S = Systems.Make (L) (Config) (Rigid_body) (Store)

  let get_pos world entity =
    let ( let* ) = Option.bind in
    let* store_packed = L.World.get_resource world Store.R.type_id in
    let* store = L.Resource.unpack_opt (module Store.R) store_packed in

    let* index_packed = L.World.get_resource world Storage.Rb_index.R.type_id in
    let* index = L.Resource.unpack_opt (module Storage.Rb_index.R) index_packed in

    match Storage.Rb_index.row_of_entity index (L.Id.Entity.to_int entity) with
    | Some row -> Some (Vec2.create store.pos_x.(row) store.pos_y.(row))
    | None -> None

  let setup app world_config =
    let config = match world_config with None -> Config.default () | Some c -> c in
    let world = L.App.world app in

    let packed = L.Resource.pack (module Config.R) config in
    L.World.add_resource Config.R.type_id packed world |> ignore;

    let body_store = Store.create ~initial:50 () in
    let packed_store = L.Resource.pack (module Store.R) body_store in
    L.World.add_resource Store.R.type_id packed_store world |> ignore;

    let rb_index = Storage.Rb_index.create ~initial:50 in
    let packed_index = L.Resource.pack (module Storage.Rb_index.R) rb_index in
    L.World.add_resource Storage.Rb_index.R.type_id packed_index world |> ignore;

    app

  let plugin ?(world_config = None) app =
    let app = setup app world_config in

    app |> L.App.on PreUpdate (S.sync_rigid_bodies ()) |> L.App.on Update (S.step ())
end
