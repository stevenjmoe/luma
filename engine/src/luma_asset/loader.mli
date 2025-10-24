open Luma__core
open Luma__ecs

(** Context providers for loader-specific contexts. *)
module Context_provider : sig
  type 'c t =
    [ `Static of 'c
    | `From_world of World.t -> ('c, Error.error) result
    ]
  (** ['c t] is a provider of a context ['c] for a loader. *)

  val static : 'a -> 'a t
  (** [static c] provides a fixed context [c]. *)

  val from_world : (World.t -> ('a, Error.error) result) -> 'a t
  (** [from_world f] provides a context computed from the current [World.t] via [f]. *)

  val no_ctx : unit t
  (** [no_ctx] provides the unit context for loaders that require none. *)
end

(** Loader interface: async decode + context-aware finalize. *)
module type LOADER = sig
  type t
  (** Asset value type produced by this loader. *)

  type decode
  (** Intermediate data produced by [begin_load]. *)

  type ctx
  (** Loader-specific context required by [finalize]. *)

  module A : Asset.S with type t = t
  (** Asset descriptor for packing the final value. *)

  val type_id : Luma__id.Id.Asset_type.t

  val exts : string list
  (** [exts] list of supported file extensions (lowercased, with dot). *)

  val begin_load : string -> k:((decode, Error.error) result -> unit) -> unit
  (** [begin_load path ~k] reads/decodes [path] off-thread and calls [k] with [decode]. *)

  val finalize : ctx -> string -> decode -> (Asset.packed, Error.error) result
  (** [finalize ctx path d] converts [d] to a packed asset using [ctx]. *)
end

(** Existential wrapper pairing a loader with its context provider. *)
type loader_packed =
  | Packed : {
      l : (module LOADER with type t = 't and type ctx = 'c);
      cp : 'c Context_provider.t;
    }
      -> loader_packed

val match_extension : (module Asset.S with type t = 'a) -> loader_packed -> path:string -> bool
(** [match_extension (module A) lp ~path] is [true] if [lp] can load [path] for asset [A]. *)
