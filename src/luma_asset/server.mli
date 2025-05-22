type load_error =
  | Unsupported_extension of string
  | Loader_error of string

type t

(** Create a new asset server that stores loaded assets in the given asset store. *)
val create : Assets.t -> t

(** Register a loader directly with the server. *)
val register_loader : t -> Loader.t -> unit

(** Load an asset from a path. Automatically dispatches to the appropriate loader based on file
    extension. Returns an asset handle or an error. *)
val load : (module Asset.S with type t = 'a) -> t -> string -> ('a Assets.handle, load_error) result

(** Global list of loader hooks registered by asset types. These are not run automatically — you
    must call [run_loader_hooks]. *)
val loader_hooks : (t -> unit) list ref

(** Register a hook that will later register one or more loaders with the asset server. *)
val register_loader_hook : (t -> unit) -> unit

(** Run all registered loader hooks against the given asset server. Typically called once after the
    server is created. *)
val run_loader_hooks : t -> unit

module R : Luma__resource.Resource.S with type t = t
