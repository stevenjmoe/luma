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
  type t = Id.Entity.t Dynarray.t

  module C = Component.Make (struct
    type inner = t

    let name = "children"
  end)
end
