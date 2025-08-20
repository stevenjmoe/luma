open Luma__id
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

type loader_packed = Packed : (module LOADER with type t = 'a) -> loader_packed

let match_extension
    (type a)
    (module A : Asset.S with type t = a)
    (Packed (module L) : loader_packed)
    ~path =
  let ext = Filename.extension path in
  List.exists (fun e -> e = ext && L.type_id = A.type_id) L.exts
