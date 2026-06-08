open Alcotest
open Luma__ecs
open Luma__resource

let ( -: ) n f = Alcotest.test_case n `Quick f

module Scene = Luma__scene.Scene.Make (Luma_driver_raylib.Driver)

let add_registries world =
  let components = Luma__type_register.Type_register.Component_registry.create () in
  let resources = Luma__type_register.Type_register.Resource_registry.create () in
  World.add_resource Luma__type_register.Type_register.Component_registry.R.type_id
    (Resource.pack (module Luma__type_register.Type_register.Component_registry.R) components)
    world
  |> World.add_resource Luma__type_register.Type_register.Resource_registry.R.type_id
       (Resource.pack (module Luma__type_register.Type_register.Resource_registry.R) resources)
  |> ignore;
  (components, resources)

let ctx_of_world_uses_distinct_registries () =
  let world = World.create () in
  let components, resources = add_registries world in
  let ctx = Scene.ctx_of_world world |> Result.get_ok in
  check bool "component registry" true (ctx.comps == components);
  check bool "resource registry" true (ctx.resources == resources)

let transform_only_round_trip () =
  let world = World.create () in
  let components, resources = add_registries world in
  let serializer =
    Luma__serialize.Serialize.pack_json (module Luma__codecs.Codecs.Json.Transform)
  in

  Luma__type_register.Type_register.Component_registry.register_component
    Luma__transform.Transform.C.name
    (module Luma__transform.Transform.C)
    [ serializer ] world;

  let transform =
    Luma__transform.Transform.create
      ~position:(Luma__math.Vec3.create 48. 72. 3.)
      ~rotation:0.25 ~scale:(Luma__math.Vec3.create 2. 3. 1.) ()
  in

  let entity : Luma__scene.Types.entity =
    {
      uuid = Uuidm.of_string "85fa1fac-e5f3-4fc7-a797-d193926436c8" |> Option.get;
      name = "level marker";
      components = [ Component.pack (module Luma__transform.Transform.C) transform ];
    }
  in

  let scene : Luma__scene.Types.t =
    {
      id = Luma__id.Id.Scene.next ();
      uuid = Uuidm.of_string "71721849-426b-45ff-8363-0d44f51491f7" |> Option.get;
      name = "transform-only";
      entities = [ entity ];
      resources = [];
      version = 1;
    }
  in
  let ctx : Luma__scene.Serialize.ctx = { comps = components; resources; version = 1 } in
  let encoded = Scene.Serialize.Json.serialize scene ctx |> Result.get_ok in
  let decoded = Scene.Serialize.Json.deserialize encoded ctx |> Result.get_ok in

  match decoded.entities with
  | [ { components = [ packed ]; _ } ] ->
      let decoded_transform =
        Component.unpack (module Luma__transform.Transform.C) packed |> Result.get_ok
      in
      check (float 0.0001) "x" 48. decoded_transform.position.x;
      check (float 0.0001) "y" 72. decoded_transform.position.y;
      check (float 0.0001) "z" 3. decoded_transform.position.z;
      check (float 0.0001) "rotation" 0.25 decoded_transform.rotation
  | _ -> fail "Expected one transform-only entity"

let tests =
  ( "scene",
    [
      "ctx_of_world uses distinct registries" -: ctx_of_world_uses_distinct_registries;
      "transform-only round trip" -: transform_only_round_trip;
    ] )
