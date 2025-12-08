(* TODO: Better errors *)
open Luma__core

let ( let* ) = Result.bind

module Serialize_value = struct
  type t =
    | Null
    | Bool of bool
    | Int of int
    | Float of float
    | String of string
    | Obj of (string * t) list
    | List of t list

  let bool ?(path = []) t = match t with Bool b -> Ok b | _ -> Error (Error.expected_bool path)
  let int ?(path = []) t = match t with Int i -> Ok i | _ -> Error (Error.expected_int path)
  let float ?(path = []) t = match t with Float f -> Ok f | _ -> Error (Error.expected_float path)

  let string ?(path = []) t =
    match t with String s -> Ok s | _ -> Error (Error.expected_string path)

  let obj ?(path = []) t = match t with Obj o -> Ok o | _ -> Error (Error.expected_obj path)
  let list ?(path = []) t = match t with List l -> Ok l | _ -> Error (Error.expected_list path)
end

module type Codec = sig
  type t

  val to_value : t -> Serialize_value.t
  val of_value : Serialize_value.t -> (t, Error.error) result
end

module type Format = sig
  type repr

  val encode : Serialize_value.t -> repr
  val decode : repr -> (Serialize_value.t, Error.error) result
  val write : out_channel -> repr -> unit
  val read : in_channel -> (repr, Error.error) result
end

module type Serializable = sig
  type t
  type repr

  val serialize : t -> repr
  val deserialize : repr -> (t, Error.error) result
end

type ('t, 'r) serializer = (module Serializable with type t = 't and type repr = 'r)
type 't serializer_pack = Json : ('t, Yojson.Safe.t) serializer -> 't serializer_pack

let pack_json r = Json r

module Json_format : Format with type repr = Yojson.Safe.t = struct
  open Yojson.Safe

  type repr = Yojson.Safe.t

  let rec encode =
    let open Serialize_value in
    function
    | Null -> `Null
    | Bool b -> `Bool b
    | Int i -> `Int i
    | Float f -> `Float f
    | String s -> `String s
    | Obj fields -> `Assoc (List.map (fun (k, v) -> (k, encode v)) fields)
    | List xs -> `List (List.map encode xs)

  let rec decode (repr : repr) : (Serialize_value.t, Error.error) result =
    let open Serialize_value in
    let rec decode_assoc fields =
      match fields with
      | [] -> Ok []
      | (k, v) :: rest ->
          let* decoded_v =
            match decode v with
            | Ok dv -> Ok dv
            | Error e -> Error (Error.decode_error ~expected:Obj [])
          in
          let* decoded_rest = decode_assoc rest in
          Ok ((k, decoded_v) :: decoded_rest)
    in

    match repr with
    | `Null -> Ok Null
    | `Bool b -> Ok (Bool b)
    | `Int i -> Ok (Int i)
    | `Float f -> Ok (Float f)
    | `String s -> Ok (String s)
    | `Assoc fields ->
        let* decoded = decode_assoc fields in
        Ok (Obj decoded)
    | `List xs ->
        let rec decode_list = function
          | [] -> Ok []
          | v :: rest ->
              let* dv =
                match decode v with
                | Ok dv -> Ok dv
                | Error e -> Error (Error.decode_error ~expected:List [])
              in
              let* dr = decode_list rest in
              Ok (dv :: dr)
        in
        let* decoded = decode_list xs in
        Ok (List decoded)
    | other -> Error (Error.decode_error ~expected:Obj [])

  let write oc json = Yojson.Safe.pretty_to_channel oc json
  let read ic = Ok (Yojson.Safe.from_channel ic)
end

module Make_serializer (F : Format) (C : Codec) :
  Serializable with type t = C.t and type repr = F.repr = struct
  type t = C.t
  type repr = F.repr

  let serialize repr = repr |> C.to_value |> F.encode

  let deserialize repr =
    let* tree = F.decode repr in
    C.of_value tree
end
