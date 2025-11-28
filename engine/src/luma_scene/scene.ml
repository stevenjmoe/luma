open Luma__app
open Luma__asset
open Luma__core
open Luma__driver
open Luma__id
open Luma__ecs
open Luma__type_register
open Luma__resource
open Types

module type S = sig
  val snapshot_world : string -> World.t -> t
  val inject_into_world : t -> World.t -> World.t
  val inject_into_world_safe : t -> World.t -> World.t
  val to_world : t -> World.t
  val write : t -> World.t -> unit
  val read : string -> string
  val add_plugin : App.t -> App.t
  val ctx_of_world : World.t -> (Serialize.ctx, Error.error) result

  module A : Asset.S with type t = Types.t
  module Serialize : module type of Serialize
end

module Make (D : Driver.S) : S = struct
  module Serialize = Serialize

  let snapshot_world name world =
    let id = Id.Scene.next () in
    let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
    let entities = World.Introspect.entities_seq world in

    let entities =
      Seq.fold_left
        (fun e_acc e_id ->
          let World.{ uuid = e_uuid; name = e_name } = World.entity_metadata world e_id in
          let components =
            Id.ComponentSet.fold
              (fun c_id c_acc ->
                match World.Introspect.get_component_packed world e_id c_id with
                | Some c -> c :: c_acc
                | None -> c_acc)
              (World.Introspect.entity_components world e_id)
              []
          in
          let (entity : entity) = { uuid = e_uuid; name = e_name; components } in
          entity :: e_acc)
        [] entities
    in

    let resources =
      World.Introspect.resources_seq world |> List.of_seq |> List.map (fun (_, packed) -> packed)
    in
    { id; uuid; name; entities; resources; version = 1 }

  let ctx_of_world world =
    match
      ( World.get_resource world Type_register.Component_registry.R.type_id,
        World.get_resource world Type_register.Resource_registry.R.type_id )
    with
    | Some comp_packed, Some res_packed -> (
        match
          ( Resource.unpack_opt (module Type_register.Component_registry.R) comp_packed,
            Resource.unpack_opt (module Type_register.Resource_registry.R) res_packed )
        with
        | Some comps, Some resources -> Ok { Serialize.comps; resources; version = 1 }
        | None, _ -> Error (Error.resource_not_found "Component_registry")
        | _, None -> Error (Error.resource_not_found "Resource_registry"))
    | _ -> Error (Error.resource_not_found "registries")

  let inject_into_world_safe scene world =
    scene.entities
    |> List.iter (fun (e : entity) ->
           if World.has_entity_uuid world e.uuid then ()
           else
             let entity = World.add_entity ~name:e.name ~uuid:(Some e.uuid) world in
             e.components |> List.iter (fun c -> World.add_component world c entity);
             ());
    world

  let inject_into_world scene world =
    scene.entities
    |> List.iter (fun (e : entity) ->
           let entity = World.add_entity ~name:e.name ~uuid:(Some e.uuid) world in
           e.components |> List.iter (fun c -> World.add_component world c entity);
           ());
    world

  let to_world scene =
    let world = World.create () in
    inject_into_world scene world

  (* TODO: *)
  let write scene world =
    match ctx_of_world world with
    | Ok ctx ->
        let file = scene.name ^ ".scn" in
        let bytes =
          Serialize.Json.serialize scene ctx
          |> Result.get_ok
          |> Yojson.Safe.pretty_to_string
          |> Bytes.of_string
        in
        D.IO.write_file file bytes
    | Error _ -> ()

  let read filepath = D.IO.read_file_blocking filepath

  module A = Asset.Make (struct
    type inner = t
  end)

  module Scene_loader :
    Loader.LOADER with type t = Types.t and type decode = bytes and type ctx = Serialize.ctx =
  struct
    type t = Types.t
    type decode = bytes
    type ctx = Serialize.ctx

    module A = A

    let type_id = A.type_id
    let exts = [ ".scn" ]

    let begin_load path ~k =
      D.IO.read_file path ~k:(function Ok bytes -> k (Ok bytes) | Error e -> k (Error e))

    let finalize ctx path bytes =
      try
        let s = Yojson.Safe.from_string (Bytes.unsafe_to_string bytes) in
        match Serialize.Json.deserialize s ctx with
        | Error e -> Error e
        | Ok v -> Ok (Asset.pack (module A) v)
      with exn -> Error (Error.io_finalize path "invalid json")
  end

  let register_loader () =
    System.make_with_resources ~components:End
      ~resources:Query.Resource.(Resource (module Server.R) & End)
      "register_scene_loader"
      (fun w _ _ (server, _) ->
        Server.register_loader server
          (module Scene_loader)
          ~ctx_provider:(Loader.Context_provider.from_world ctx_of_world);
        w)

  let add_plugin app = app |> App.on Startup @@ register_loader ()
end
