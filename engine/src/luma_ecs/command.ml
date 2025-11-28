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
  | Insert
  | Remove
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

let flush world buf =
  List.iter
    (fun cmd ->
      match cmd with
      | Spawn { entity; name; components } ->
          let e_id = World.register_entity world entity name in
          World.add_components world e_id components;
          ()
      | Despawn _ -> ()
      | Insert -> ()
      | Remove -> ()
      | Insert_resource -> ()
      | Remove_resource -> ())
    buf.commands
