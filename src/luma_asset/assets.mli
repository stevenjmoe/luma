type t = (Luma__id.Id.Asset.t, Asset.packed list) Hashtbl.t

val create : unit -> ('a, 'b) Hashtbl.t

val add :
  (Luma__id.Id.Asset.t, Asset.packed list) Hashtbl.t ->
  (module Asset.S with type t = 'a) ->
  'a ->
  unit

val get_all :
  (Luma__id.Id.Asset.t, Asset.packed list) Hashtbl.t -> (module Asset.S with type t = 'a) -> 'a list

module Texture_atlas : sig
  type t

  module R : Asset.S with type t = t
end

module Texture : sig
  type t

  module R : Asset.S with type t = t
end

module R : Luma__resource.Resource.S with type t = t
