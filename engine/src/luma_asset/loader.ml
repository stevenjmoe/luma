open Luma__core
open Luma__ecs

module type LOADER = sig
  type t
  type decode
  type ctx

  module A : Asset.S with type t = t

  val type_id : Luma__id.Id.Asset_type.t
  val exts : string list
  val begin_load : string -> k:((decode, Error.error) result -> unit) -> unit
  val finalize : ctx -> string -> decode -> (Asset.packed, Error.error) result
end

module Context_provider = struct
  type 'c t =
    [ `Static of 'c
    | `From_world of World.t -> ('c, Error.error) result
    ]

  let static x = `Static x
  let from_world f = `From_world f
  let no_ctx = `Static ()
end

type loader_packed =
  | Packed : {
      l : (module LOADER with type t = 't and type ctx = 'c);
      cp : 'c Context_provider.t;
    }
      -> loader_packed

let match_extension
    (type a)
    (module A : Asset.S with type t = a)
    (Packed { l = (module L); _ } : loader_packed)
    ~path =
  let ext = Filename.extension path in
  let exists = List.exists (fun e -> e = ext && L.type_id = A.type_id) L.exts in
  exists
