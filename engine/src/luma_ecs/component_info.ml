open Luma__id

module Component_hook = struct
  type t = entity:Id.Entity.t -> command:Command.t -> component:Component.packed -> unit

  let make
      (type a)
      (module C : Component.S with type t = a)
      (f : entity:Id.Entity.t -> command:Command.t -> component:a -> unit) =
   fun ~entity ~command ~component ->
    match Component.unpack_opt (module C) component with
    | Some value -> f ~entity ~command ~component:value
    | None -> failwith "Component_hook: component hook type mismatch"
end

module Component_hooks = struct
  type t = {
    on_insert : Component_hook.t option;
    on_discard : Component_hook.t option;
  }

  let empty = { on_insert = None; on_discard = None }
end

type t = {
  component : Id.Component.t;
  hooks : Component_hooks.t;
}
