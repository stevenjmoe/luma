open Luma__core

module type LOADER = sig
  type t
  type decode

  module A : Asset.S with type t = t

  val type_id : Luma__id.Id.Asset_type.t
  val exts : string list
  val begin_load : string -> k:((decode, string) result -> unit) -> unit
  val finalize : string -> decode -> (Asset.packed, string) result
end

(** A single-variant GADT that packages a dynamically typed asset.

    The pair ([(module A) * value]) represents a first-class module [A] which implements [Asset.S],
    with some hidden type ['a], and a value of the same type ['a]. *)

type loader_packed = Packed : (module LOADER with type t = 'a) -> loader_packed

(** [exts]: A list of the file extensions that this loader supports, without the fullstop.

    [load path load_fn] should accept a path and return [Ok loaded] if the file was successfully
    loaded, otherwise [Error msg] *)

val match_extension : (module Asset.S with type t = 'a) -> loader_packed -> path:string -> bool
