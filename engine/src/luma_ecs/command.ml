open Luma__id
open Luma__resource

type spawn_request = {
  entity : Entity.t;
  name : string;
  components : Component.packed list;
}

type command =
  | Spawn of spawn_request
  | Despawn of Id.Entity.t
  | Insert of Id.Entity.t * Component.packed
  | Remove of Id.Entity.t * Id.Component.t
  | Insert_resource of Resource.packed
  | Remove_resource of Id.Resource.t

type t = { mutable commands : command list }

let create () = { commands = [] }
let commands c = c.commands

let spawn_packed ?(name = "") ?uuid buf comps =
  let entity = Entity.make name ~uuid in
  buf.commands <- Spawn { entity; name; components = comps } :: buf.commands;
  Entity.id entity

let spawn ?(name = "") ?uuid buf comps =
  let components =
    List.map (fun (Component.Component ((module C), v)) -> Component.pack (module C) v) comps
  in
  spawn_packed ~name ?uuid buf components

let despawn buf entity = buf.commands <- Despawn entity :: buf.commands

let insert (type a) buf entity (module C : Component.S with type t = a) c =
  let packed = Component.pack (module C) c in
  buf.commands <- Insert (entity, packed) :: buf.commands

let remove buf entity component = buf.commands <- Remove (entity, component) :: buf.commands

let insert_resource (type a) buf (module R : Resource.S with type t = a) res =
  let packed = Resource.pack (module R) res in
  buf.commands <- Insert_resource packed :: buf.commands

let remove_resource buf res = buf.commands <- Remove_resource res :: buf.commands

let flush world buf =
  List.iter
    (fun cmd ->
      match cmd with
      | Spawn { entity; name; components } ->
          let e_id = World.register_entity world entity name in
          World.add_components world e_id components;
          ()
      | Despawn e -> World.remove_entity world e
      | Insert (e, c) -> World.add_component world c e
      | Remove (e, c) -> World.remove_component world c e
      | Insert_resource r -> ignore (World.add_resource (Resource.type_id r) r world)
      | Remove_resource r -> World.remove_resource r world)
    buf.commands
