open Luma__core
open Luma__ecs

module type L = sig
  type t
  type ctx

  module A : Asset.S with type t = t

  val exts : string list
  val begin_load : string -> int
  val finalize : ctx -> string -> bytes -> (Asset.packed, Error.error) result
end

module type LOADER = sig
  type t
  type ctx

  module A : Asset.S with type t = t

  val type_id : Luma__id.Id.Asset_type.t
  val exts : string list
  val begin_load : string -> int
  val finalize : ctx -> string -> bytes -> (Asset.packed, Error.error) result
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
      loader : (module LOADER with type t = 't and type ctx = 'c);
      ctx_provider : 'c Context_provider.t;
    }
      -> loader_packed

let match_extension
    (type a)
    (module A : Asset.S with type t = a)
    (Packed { loader = (module L); _ } : loader_packed)
    ~path =
  let ext = Filename.extension path in
  let exists = List.exists (fun e -> e = ext && L.type_id = A.type_id) L.exts in
  exists

module Make (L : L) : LOADER with type t = L.t and type ctx = L.ctx = struct
  type t = L.t
  type ctx = L.ctx

  module A = L.A

  let exts = L.exts
  let type_id = A.type_id
  let begin_load = L.begin_load
  let finalize = L.finalize
end
