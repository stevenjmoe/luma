module Time = struct
  type t = { mutable dt : float; mutable elapsed : float }

  let dt t = t.dt

  module R = Resource.Make (struct
    type inner = t
  end)
end

module Texture_atlas = struct
  type t = (string, Luma__image.Image.Texture_atlas.t) Hashtbl.t

  let create () = Hashtbl.create 10
  let get_texture_atlas t key = Hashtbl.find_opt t key

  module R = Resource.Make (struct
    type inner = t
  end)
end

module Texture = struct
  type t = (string, Luma__texture.Texture.t) Hashtbl.t

  let create () = Hashtbl.create 10
  let get_texture t key = Hashtbl.find_opt t key

  module R = Resource.Make (struct
    type inner = t
  end)
end
