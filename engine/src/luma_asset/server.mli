type t

val create : Assets.t -> t
(** Create a new asset server that stores loaded assets in the given asset store. *)

val register_loader :
  t ->
  (module Loader.LOADER with type ctx = 'c and type t = 't) ->
  ctx_provider:'c Loader.Context_provider.t ->
  unit
(** Register a loader directly with the server. *)

val load :
  (module Asset.S with type t = 'a) ->
  t ->
  string ->
  Luma__ecs.World.t ->
  (Assets.handle, Luma__core.Error.error) result
(** Load an asset from a path. Automatically dispatches to the appropriate loader based on file
    extension. Returns an asset handle or an error. *)

val loader_hooks : (t -> unit) list ref
(** Global list of loader hooks registered by asset types. These are not run automatically — you
    must call [run_loader_hooks]. *)

val register_loader_hook : (t -> unit) -> unit
(** Register a hook that will later register one or more loaders with the asset server. *)

val run_loader_hooks : t -> unit
(** Run all registered loader hooks against the given asset server. Typically called once after the
    server is created. *)

module R : Luma__resource.Resource.S with type t = t
