type asset_record = {
  packed : Asset.packed;
  generation : int;
  type_id : Luma__id.Id.Asset_type.t;
}
(** Represents a single stored asset instance. *)

type t = (Luma__id.Id.Asset.t, asset_record) Hashtbl.t
(** The central store for all loaded assets, keyed by an asset ID. *)

type 'a handle = {
  id : Luma__id.Id.Asset.t;
  type_id : Luma__id.Id.Asset_type.t;
  generation : int;
}
(** A phantom-typed handle referencing a loaded asset of type ['a]. *)

val create : unit -> t
(** Create a new, empty asset store. *)

val add :
  (module Asset.S with type t = 'a) ->
  t ->
  id:Luma__id.Id.Asset.t ->
  generation:int ->
  asset:'a ->
  'b handle
(** Add a typed asset to the store, returning a typed handle. *)

val get : (module Asset.S with type t = 'a) -> t -> 'a handle -> 'a option
(** Retrieve a typed asset from the store by handle. Returns [None] if missing or stale. *)

val unload : t -> 'a handle -> unit
(** Unload an asset from the store by handle. *)

module Texture_atlas : sig
  type t

  module R : Asset.S with type t = t
end

module Texture : sig
  type t = Raylib.Texture.t

  module A : Asset.S with type t = t
end

module R : Luma__resource.Resource.S with type t = t
