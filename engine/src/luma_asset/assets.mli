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
  path : string option;
}
(** Represents a single stored asset instance. *)

type t = (Id.Asset.t, asset_record) Hashtbl.t
(** The central store for all loaded assets, keyed by an asset ID. *)

type handle = {
  id : Id.Asset.t;
  type_id : Id.Asset_type.t;
  generation : int;
  path : string option;
}
(** A handle referencing a loaded asset. *)

val create : unit -> t
(** Create a new, empty asset store. *)

val add_pending :
  (module Asset.S with type t = 'a) ->
  ?path:string ->
  (Id.Asset.t, asset_record) Hashtbl.t ->
  handle
(** Insert a placeholder in [Loading] state for a future resolve. *)

val add :
  (module Asset.S with type t = 'a) ->
  ?path:string ->
  (Id.Asset.t, asset_record) Hashtbl.t ->
  'a ->
  handle
(** Insert a ready asset value. Returns its handle. *)

val get : (module Asset.S with type t = 'a) -> t -> handle -> 'a option
(** Get a typed value if present and generations match, else [None]. *)

val get_all : (module Asset.S with type t = 'a) -> t -> 'a list
(** Collect all ready assets of this type. *)

val exists : t -> handle -> bool
(** [true] if an entry with the same [id] and [generation] exists. *)

val is_loaded : t -> handle -> bool
(** [true] if the entry exists (same generation) and is [Ready]. *)

val unload : t -> handle -> unit
(** Unload an asset from the store by handle. *)

val resolve : (module Asset.S with type t = 'a) -> t -> handle -> Asset.packed -> unit
(** Resolve a pending asset with a pre-packed payload. *)

val fail : t -> handle -> failed -> unit
(** Mark an entry as [Failed]. *)

module R : Luma__resource.Resource.S with type t = t

(** Typed facade for a concrete asset type [A]. Removes the need to pass [(module A)] at each
    call-site and reduces mismatches. Storage remains the same untyped registry.

    {2 Example}

    {[
      (* Define an asset kind *)
      module Texture_asset = Asset.Make (struct
        type inner = L.Texture.t
      end)

      (* Get a typed facade *)
      module Assets = Assets.For (Texture_asset)

      (* Insert a ready value *)
      let h = Assets.add assets texture in

      (* Read it back, typed *)
      let () =
        match Assets.get assets h with
        | Some t ->
            (* use [t : L.Texture.t] *)
            ()
        | None -> ()
    ]}*)
module type For = sig
  type nonrec t = t
  (** Asset store. *)

  type nonrec handle = handle
  (** Handle to an asset entry (includes id, type_id, generation, path). *)

  type asset

  val add : ?path:string -> t -> asset -> handle
  (** Insert a ready asset value of type [A.t]. Returns its handle. *)

  val add_pending : ?path:string -> t -> handle
  (** Insert a placeholder in [Loading] state for a future resolve. *)

  val resolve : t -> handle -> Asset.packed -> unit
  (** Resolve a pending asset with a pre-packed payload. *)

  val fail : t -> handle -> failed -> unit
  (** Mark an entry as [Failed]. *)

  val get : t -> handle -> asset option
  (** Get a typed value if present and generations match, else [None]. *)

  val get_all : t -> asset list
  (** Collect all ready assets of this type. *)

  val exists : t -> handle -> bool
  (** [true] if an entry with the same [id] and [generation] exists. *)

  val is_loaded : t -> handle -> bool
  (** [true] if the entry exists (same generation) and is [Ready]. *)

  val unload : t -> handle -> unit
  (** Remove the entry if generations match. *)
end

module For : functor (A : Asset.S) ->
  For with type t = t and type handle = handle and type asset = A.t
