module type Format = sig
  type repr

  val write : out_channel -> repr -> unit
  val read : in_channel -> (repr, string) result
end

module type Serializable = sig
  type t
  type repr

  val serialize : t -> repr
  val deserialize : repr -> (t, string) result
end

type ('t, 'r) serializer = (module Serializable with type t = 't and type repr = 'r)
type 't serializer_pack = Json : ('t, Yojson.Safe.t) serializer -> 't serializer_pack

let pack_json r = Json r

module Json_format : Format with type repr = Yojson.Safe.t = struct
  open Yojson.Safe

  type repr = Yojson.Safe.t

  let write oc json = Yojson.Safe.pretty_to_channel oc json
  let read ic = Ok (Yojson.Safe.from_channel ic)
end

module Make_serializer
    (F : Format)
    (S : sig
      type t

      val to_repr : t -> F.repr
      val of_repr : F.repr -> (t, string) result
    end) : Serializable with type t = S.t and type repr = F.repr = struct
  type t = S.t
  type repr = F.repr

  let serialize = S.to_repr
  let deserialize = S.of_repr
end
