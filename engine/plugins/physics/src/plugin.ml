open Luma__resource
open Luma__app

module type S = sig
  val plugin : ?world_config:Config.t -> App.t -> App.t
end

module Make (L : Luma.S) : S = struct
  module S = Systems.Make (L)

  let add_config config world =
    let packed = Resource.pack (module Config.R) config in
    Luma__ecs.World.add_resource Config.R.type_id packed world

  let add_rb_store world =
    let body_store = Rb_store.create ~initial:50 () in
    let packed_store = Resource.pack (module Rb_store.R) body_store in
    Luma__ecs.World.add_resource Rb_store.R.type_id packed_store world

  let add_rb_index world =
    let rb_index = Rb_store.Index.create ~initial:50 in
    let packed_index = Resource.pack (module Rb_store.Index.R) rb_index in
    Luma__ecs.World.add_resource Rb_store.Index.R.type_id packed_index world

  let add_grid bounds world =
    let grid = Grid.create bounds 2. in
    let packed_grid = Resource.pack (module Grid.R) grid in
    Luma__ecs.World.add_resource Grid.R.type_id packed_grid world

  let add_broad_phase max_bodies world =
    let broad_phase = Broad_phase.create ~max_bodies () in
    let packed_bp = Resource.pack (module Broad_phase.R) broad_phase in
    Luma__ecs.World.add_resource Broad_phase.R.type_id packed_bp world

  let add_narrow_phase world =
    let narrow_phase = Narrow_phase.create () in
    let packed_np = Resource.pack (module Narrow_phase.R) narrow_phase in
    Luma__ecs.World.add_resource Narrow_phase.R.type_id packed_np world

  let add_event_store world =
    let ces = Collision_event.Collision_events_store.create () in
    let packed_ces = Resource.pack (module Collision_event.Collision_events_store.R) ces in
    Luma__ecs.World.add_resource Collision_event.Collision_events_store.R.type_id packed_ces world

  let add_resources (config : Config.t) world =
    add_config config world
    |> add_rb_store
    |> add_rb_index
    |> add_grid config.bounds
    |> add_broad_phase config.max_bodies
    |> add_narrow_phase
    |> add_event_store

  let setup app config =
    Luma__app.App.world app |> add_resources config |> ignore;
    app

  let plugin ?world_config app =
    let config = match world_config with None -> Config.default () | Some c -> c in
    let app = setup app config in
    let packed_rb_serializer =
      Luma__serialize.Serialize.pack_json (module Serialize.Rigid_body_serializer)
    in
    App.register_component Rigid_body.C.name (module Rigid_body.C) [ packed_rb_serializer ] app
    |> ignore;
    let packed_collider_serializer =
      Luma__serialize.Serialize.pack_json (module Serialize.Colliders_serializer)
    in
    App.register_component Colliders.C.name (module Colliders.C) [ packed_collider_serializer ] app
    |> ignore;

    app
    |> App.on PreUpdate (S.sync_to_store ())
    |> App.on PreUpdate (S.sync_from_store ())
    |> App.on Update (S.step ())
    |> App.on PreRender ~run_if:(fun _ -> config.debug = true) (S.debug_draw ())
end
