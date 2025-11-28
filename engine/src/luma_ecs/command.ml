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
  | Insert_resource
  | Remove_resource

type t = { mutable commands : command list }

let create () = { commands = [] }
let commands c = c.commands

let spawn ?(name = "") ?uuid buf comps =
  let entity = Entity.make name ~uuid in
  let components =
    List.map (fun (Component.Component ((module C), v)) -> Component.pack (module C) v) comps
  in
  buf.commands <- Spawn { entity; name; components } :: buf.commands;
  Entity.id entity

let insert (type a) buf entity (module C : Component.S with type t = a) c =
  let packed = Component.pack (module C) c in
  buf.commands <- Insert (entity, packed) :: buf.commands

let remove buf entity component = buf.commands <- Remove (entity, component) :: buf.commands

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
      | Insert_resource -> ()
      | Remove_resource -> ())
    buf.commands
