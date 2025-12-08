open Luma__core
open Luma__math
open Luma__resource
open Luma__ecs

module type S = sig
  type app

  module Rigid_body = Rigid_body
  module Colliders = Colliders

  val setup : app -> Config.t -> app
  val pos : Luma__ecs.World.t -> Luma__id.Id.EntitySet.elt -> Vec2.t option
  val plugin : ?world_config:Config.t -> app -> app
end

module Make (L : Luma.S) : S with type app = L.App.t = struct
  module S = Systems.Make (L)
  module Rigid_body = Rigid_body
  module Colliders = Colliders

  type app = L.App.t

  (* Private *)

  let add_config config world =
    let packed = Resource.pack (module Config.R) config in
    World.add_resource Config.R.type_id packed world

  let add_rb_store world =
    let body_store = Rb_store.create ~initial:50 () in
    let packed_store = Resource.pack (module Rb_store.R) body_store in
    World.add_resource Rb_store.R.type_id packed_store world

  let add_rb_index world =
    let rb_index = Rb_store.Index.create ~initial:50 in
    let packed_index = Resource.pack (module Rb_store.Index.R) rb_index in
    World.add_resource Rb_store.Index.R.type_id packed_index world

  let add_grid bounds world =
    let grid = Grid.create bounds 2. in
    let packed_grid = Resource.pack (module Grid.R) grid in
    World.add_resource Grid.R.type_id packed_grid world

  let add_broad_phase world =
    let broad_phase = Broad_phase.create () in
    let packed_bp = Resource.pack (module Broad_phase.R) broad_phase in
    World.add_resource Broad_phase.R.type_id packed_bp world

  let add_narrow_phase world =
    let narrow_phase = Narrow_phase.create () in
    let packed_np = Resource.pack (module Narrow_phase.R) narrow_phase in
    World.add_resource Narrow_phase.R.type_id packed_np world

  let add_event_store world =
    let ces = Collision_event.Collision_events_store.create () in
    let packed_ces = Resource.pack (module Collision_event.Collision_events_store.R) ces in
    World.add_resource Collision_event.Collision_events_store.R.type_id packed_ces world

  let add_resources (config : Config.t) world =
    add_config config world
    |> add_rb_store
    |> add_rb_index
    |> add_grid config.bounds
    |> add_broad_phase
    |> add_narrow_phase
    |> add_event_store

  let setup app config =
    L.App.world app |> add_resources config |> ignore;
    app

  (* Public *)

  let pos world entity =
    let ( let* ) = Option.bind in
    let* store_packed = World.get_resource world Rb_store.R.type_id in
    let* store = Resource.unpack_opt (module Rb_store.R) store_packed in

    let* index_packed = World.get_resource world Rb_store.Index.R.type_id in
    let* index = Resource.unpack_opt (module Rb_store.Index.R) index_packed in

    match Rb_store.Index.row_of_entity index entity with
    | Some row -> Some (Vec2.create store.pos_x.(row) store.pos_y.(row))
    | None -> None

  let plugin ?world_config app =
    let config = match world_config with None -> Config.default () | Some c -> c in
    let app = setup app config in
    let packed_rb_serializer =
      Luma__serialize.Serialize.pack_json (module Serialize.Rigid_body_serializer)
    in
    L.App.register_component Rigid_body.C.name (module Rigid_body.C) [ packed_rb_serializer ] app
    |> ignore;
    let packed_collider_serializer =
      Luma__serialize.Serialize.pack_json (module Serialize.Colliders_serializer)
    in
    L.App.register_component Colliders.C.name
      (module Colliders.C)
      [ packed_collider_serializer ] app
    |> ignore;

    app
    |> L.App.on PreUpdate (S.sync_rigid_bodies ())
    |> L.App.on Update (S.step ())
    |> L.App.on PreRender ~run_if:(fun w -> config.debug = true) (S.debug_draw ())
end
