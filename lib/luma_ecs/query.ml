module Filter = struct
  type t =
    | With of Id.Component.t
    | Without of Id.Component.t
    | Not of t
    | And of t * t
    | Or of t * t
    | Any

  let ( & ) f1 f2 = And (f1, f2)

  let rec matches f components =
    match f with
    | With c -> Id.ComponentSet.mem c components
    | Without c -> not (Id.ComponentSet.mem c components)
    | Not f -> not (matches f components)
    | And (f1, f2) -> matches f2 components && matches f2 components
    | Or (f1, f2) -> matches f1 components || matches f2 components
    | Any -> true
end

type _ term =
  | Required : (module Component.S with type t = 'a) -> 'a term
  | Optional : (module Component.S with type t = 'a) -> 'a option term

type _ t = Query : ('a term * 'b t) -> ('a * 'b) t | End : unit t

let ( & ) query rest = Query (query, rest)

let rec required_ids : type a. a t -> _ = function
  | Query (Required (module C), rest) -> Id.ComponentSet.add C.id (required_ids rest)
  | Query (Optional (module C), rest) -> required_ids rest
  | End -> Id.ComponentSet.empty

let evaluate : type a. ?filter:Filter.t -> a t -> Archetype.t list -> (Id.Entity.t * a) list =
 fun ?(filter = Filter.Any) query archetypes ->
  let matches = Filter.matches filter in
  let rec fetch : type a. a t -> Archetype.t -> Id.Entity.t -> a =
   fun query arch entity ->
    match query with
    | End -> ()
    | Query (Required (module C), rest) -> (
        match Archetype.query_table arch entity C.id with
        | Some c -> (
            let unpacked = Component.unpack (module C) c in
            match unpacked with
            | Some u -> (u, fetch rest arch entity)
            | None -> failwith "TODO: unpacked not found.")
        | None -> failwith "TODO: arch not found")
    | Query (Optional (module C), rest) -> (
        match Archetype.query_table arch entity C.id with
        | Some c ->
            let unpacked = Component.unpack (module C) c in
            (unpacked, fetch rest arch entity)
        | None -> (None, fetch rest arch entity))
  in
  archetypes
  |> List.filter (fun a ->
         let components = Archetype.components a in
         let required_ids = required_ids query in
         matches components && Id.ComponentSet.subset required_ids components)
  |> List.concat_map (fun a ->
         Archetype.entities a |> Id.EntitySet.to_list |> List.map (fun e -> (e, fetch query a e)))
