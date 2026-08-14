open Luma__id
open Luma__resource

type spawn_request = {
  entity : Entity.t;
  name : string;
  components : Component.packed list;
}

type 'a entry_modification = {
  entity : Id.Entity.t;
  component : (module Component.S with type t = 'a);
  f : 'a option -> 'a option;
}

type command =
  | Spawn of spawn_request
  | Despawn of Id.Entity.t
  | Insert of Id.Entity.t * Component.packed
  | Remove of Id.Entity.t * Id.Component.t
  | Modify_entry : 'a entry_modification -> command
  | Insert_resource of Resource.packed
  | Remove_resource of Id.Resource.t

type t = { mutable commands : command list }

let create () = { commands = [] }
let commands c = c.commands

let take buf =
  let commands = buf.commands in
  buf.commands <- [];
  commands

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

let modify_entry (type a) (component : (module Component.S with type t = a)) entity f buf =
  buf.commands <- Modify_entry { entity; component; f } :: buf.commands

let insert_resource (type a) buf (module R : Resource.S with type t = a) res =
  let packed = Resource.pack (module R) res in
  buf.commands <- Insert_resource packed :: buf.commands

let remove_resource buf res = buf.commands <- Remove_resource res :: buf.commands
