(** Represents a single stored asset instance. *)
type asset_record = {
  packed : Asset.packed;
  generation : int;
  type_id : Luma__id.Id.Asset_type.t;
}

(** The central store for all loaded assets, keyed by an asset ID. *)
type t = (Luma__id.Id.Asset.t, asset_record) Hashtbl.t

(** A phantom-typed handle referencing a loaded asset of type ['a]. *)
type 'a handle = {
  id : Luma__id.Id.Asset.t;
  type_id : Luma__id.Id.Asset_type.t;
  generation : int;
}

(** Create a new, empty asset store. *)
val create : unit -> t

(** Add a typed asset to the store, returning a typed handle. *)
val add :
  (module Asset.S with type t = 'a) ->
  (Luma__id__Id.Asset.t, asset_record) Hashtbl.t ->
  'a ->
  'b handle

(** Retrieve a typed asset from the store by handle. Returns [None] if missing or stale. *)
val get : (module Asset.S with type t = 'a) -> t -> 'a handle -> 'a option

(** Retrieve all assets of where the record's [type_id] matches the provided [Asset] [type_id]. *)
val get_all : (module Asset.S with type t = 'a) -> t -> 'a list

(** Unload an asset from the store by handle. *)
val unload : t -> 'a handle -> unit

module R : Luma__resource.Resource.S with type t = t
