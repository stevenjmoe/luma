open Luma__id

type failed = {
  path : string;
  msg : string;
}

type status =
  | Loading
  | Ready of Asset.packed
  | Failed of failed

type asset_record = {
  mutable status : status;
  generation : int;
  type_id : Id.Asset_type.t;
}
(** Represents a single stored asset instance. *)

type t = (Id.Asset.t, asset_record) Hashtbl.t
(** The central store for all loaded assets, keyed by an asset ID. *)

type 'a handle = {
  id : Id.Asset.t;
  type_id : Id.Asset_type.t;
  generation : int;
  path : string option;
}
(** A phantom-typed handle referencing a loaded asset of type ['a]. *)

val create : unit -> t
(** Create a new, empty asset store. *)

val add_pending :
  (module Asset.S with type t = 'a) ->
  ?path:string ->
  (Id.Asset.t, asset_record) Hashtbl.t ->
  'b handle

val add :
  (module Asset.S with type t = 'a) ->
  ?path:string ->
  (Id.Asset.t, asset_record) Hashtbl.t ->
  'a ->
  'b handle
(** Add a typed asset to the store, returning a typed handle. *)

val get : (module Asset.S with type t = 'a) -> t -> 'a handle -> 'a option
(** Retrieve a typed asset from the store by handle. Returns [None] if missing or stale. *)

val get_all : (module Asset.S with type t = 'a) -> t -> 'a list
(** Retrieve all assets of where the record's [type_id] matches the provided [Asset] [type_id]. *)

val is_loaded : t -> _ handle -> bool

val unload : t -> 'a handle -> unit
(** Unload an asset from the store by handle. *)

val resolve : (module Asset.S with type t = 'a) -> t -> 'a handle -> Asset.packed -> unit
val fail : t -> 'a handle -> failed -> unit

module R : Luma__resource.Resource.S with type t = t
