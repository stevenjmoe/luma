open Luma__id

module ChildOf = struct
  type t = ChildOf of Id.Entity.t

  let create parent = ChildOf parent
  let parent (ChildOf parent) = parent

  module C = Component.Make (struct
    type inner = t

    let name = "child_of"
  end)
end

module Children = struct
  type t = Id.Entity.t list

  module C = Component.Make (struct
    type inner = t

    let name = "children"
  end)
end

let on_child_of_insert ~entity:child ~command ~component =
  let parent = ChildOf.parent component in

  Command.modify_entry
    (module Children.C)
    parent
    (function None -> Some [ child ] | Some children -> Some (child :: children))
    command

let on_child_of_discard ~entity:child ~command ~component =
  let parent = ChildOf.parent component in

  Command.modify_entry
    (module Children.C)
    parent
    (function
      | None -> None
      | Some children ->
          let children = children |> List.filter (fun c -> not @@ Id.Entity.eq child c) in
          if children = [] then None else Some children)
    command

let child_of_hooks : Component_info.Component_hooks.t =
  {
    on_insert = Some (Component_info.Component_hook.make (module ChildOf.C) on_child_of_insert);
    on_discard = Some (Component_info.Component_hook.make (module ChildOf.C) on_child_of_discard);
  }
