module type Format = sig
  type repr

  val write : out_channel -> repr -> unit
  val read : in_channel -> (repr, string) result
  val null : repr
  val bool : bool -> repr
  val int : int -> repr
  val float : float -> repr
  val string : string -> repr
  val list : repr list -> repr
  val obj : (string * repr) list -> repr
end

module type Serializable = sig
  type t
  type repr

  val name : string
  val serialize : t -> repr
  val deserialize : repr -> (t, string) result
end

type ('t, 'r) serialize = {
  serialize : 't -> 'r;
  deserialize : 'r -> ('t, string) result;
}

type _ fmt = Json : Yojson.Safe.t fmt

type 't serializer_pack =
  | Serializer :
      (module Serializable with type t = 't and type repr = 'r) * 'r fmt
      -> 't serializer_pack

let pack_json (type t) (module S : Serializable with type t = t and type repr = Yojson.Safe.t) =
  Serializer ((module S), Json)

module Json_format : Format with type repr = Yojson.Safe.t = struct
  open Yojson.Safe

  type repr = Yojson.Safe.t

  let write oc json = Yojson.Safe.pretty_to_channel oc json
  let read ic = Ok (Yojson.Safe.from_channel ic)
  let null = `Null
  let bool = fun b -> `Bool b
  let int = fun i -> `Int i
  let float = fun f -> `Float f
  let string = fun s -> `String s
  let list x = `List x
  let obj fields = `Assoc fields
end

module Make_json_serializer (T : sig
  type t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end) : Serializable with type t = T.t and type repr = Yojson.Safe.t = struct
  type t = T.t
  type repr = Yojson.Safe.t

  let name = "json"
  let serialize = T.to_yojson
  let deserialize = T.of_yojson
end
