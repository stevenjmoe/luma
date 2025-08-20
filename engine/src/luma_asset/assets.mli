type status =
  | Loading
  | Ready of Asset.packed
  | Failed of string

type asset_record = {
  mutable status : status;
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

val add_pending :
  (module Asset.S with type t = 'a) -> (Luma__id__Id.Asset.t, asset_record) Hashtbl.t -> 'b handle

val add :
  (module Asset.S with type t = 'a) ->
  (Luma__id__Id.Asset.t, asset_record) Hashtbl.t ->
  'a ->
  'b handle
(** Add a typed asset to the store, returning a typed handle. *)

val get : (module Asset.S with type t = 'a) -> t -> 'a handle -> 'a option
(** Retrieve a typed asset from the store by handle. Returns [None] if missing or stale. *)

val get_all : (module Asset.S with type t = 'a) -> t -> 'a list
(** Retrieve all assets of where the record's [type_id] matches the provided [Asset] [type_id]. *)

val unload : t -> 'a handle -> unit
(** Unload an asset from the store by handle. *)

val resolve : (module Asset.S with type t = 'a) -> t -> 'a handle -> Asset.packed -> unit
val fail : t -> 'a handle -> string -> unit

module R : Luma__resource.Resource.S with type t = t
