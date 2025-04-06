type asset_record = { packed : Asset.packed; generation : int }
type t = (Luma__id.Id.Asset.t, asset_record) Hashtbl.t
type handle = { id : Luma__id.Id.Asset.t; generation : int }

val create : unit -> (Luma__id.Id.Asset.t, asset_record) Hashtbl.t

val add :
  (Luma__id.Id.Asset.t, asset_record) Hashtbl.t ->
  id:Luma__id.Id.Asset.t ->
  packed:Asset.packed ->
  generation:int ->
  unit

val get : (module Asset.S with type t = 'a) -> t -> handle -> 'a option
val unload : t -> handle -> unit

module Texture_atlas : sig
  type t

  module R : Asset.S with type t = t
end

module Texture : sig
  type t = Raylib.Texture.t

  module A : Asset.S with type t = t
end

module R : Luma__resource.Resource.S with type t = t
