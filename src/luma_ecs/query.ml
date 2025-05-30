module Component = struct
  module Filter = struct
    type t =
      | With of Luma__id.Id.Component.t
      | Without of Luma__id.Id.Component.t
      | Not of t
      | And of t * t
      | Or of t * t
      | Any

    let ( & ) f1 f2 = And (f1, f2)

    let rec matches f components =
      match f with
      | With c -> Luma__id.Id.ComponentSet.mem c components
      | Without c -> not (Luma__id.Id.ComponentSet.mem c components)
      | Not f -> not (matches f components)
      | And (f1, f2) -> matches f1 components && matches f2 components
      | Or (f1, f2) -> matches f1 components || matches f2 components
      | Any -> true
  end

  type _ term =
    | Required : (module Component.S with type t = 'a) -> 'a term
    | Optional : (module Component.S with type t = 'a) -> 'a option term

  type _ t =
    | Query : ('a term * 'b t) -> ('a * 'b) t
    | End : unit t

  let ( & ) query rest = Query (query, rest)

  let rec required_ids : type a. a t -> _ = function
    | Query (Required (module C), rest) -> Luma__id.Id.ComponentSet.add C.id (required_ids rest)
    | Query (Optional (module C), rest) -> required_ids rest
    | End -> Luma__id.Id.ComponentSet.empty

  (* TODO: Not too happy with handling with this in an exception but it was the fastest approach. Fix later!*)
  exception Eval_exn of Luma__core.Error.error

  let evaluate : type a.
      ?filter:Filter.t ->
      a t ->
      Archetype.t list ->
      ((Luma__id.Id.Entity.t * a) list, Luma__core.Error.error) result =
   fun ?(filter = Filter.Any) query archetypes ->
    let matches = Filter.matches filter in
    let rec fetch : type a. a t -> Archetype.t -> Luma__id.Id.Entity.t -> a =
     fun query arch entity ->
      match query with
      | End -> ()
      | Query (Required (module C), rest) -> (
          match Archetype.query_table arch entity C.id with
          | Some c -> (
              match Component.unpack (module C) c with
              | Ok u -> (u, fetch rest arch entity)
              | Error e -> raise @@ Eval_exn e)
          | None ->
              raise
              @@ Eval_exn
                   (Luma__core.Error.component_not_found
                      (Luma__id.Id.Component.to_int C.id)
                      (Archetype.hash arch) "Could not evaluate query."))
      | Query (Optional (module C), rest) -> (
          match Archetype.query_table arch entity C.id with
          | Some c -> (
              match Component.unpack (module C) c with
              | Ok u -> (Some u, fetch rest arch entity)
              | Error _ -> (None, fetch rest arch entity))
          | None -> (None, fetch rest arch entity))
    in
    try
      Ok
        (archetypes
        |> List.filter (fun a ->
               let components = Archetype.components a in
               let required_ids = required_ids query in
               matches components && Luma__id.Id.ComponentSet.subset required_ids components)
        |> List.concat_map (fun a ->
               Archetype.entities a
               |> Luma__id.Id.EntitySet.to_list
               |> List.map (fun e -> (e, fetch query a e))))
    with Eval_exn e -> Error e
end

module Resource = struct
  open Luma__resource.Resource

  type _ term = Resource : (module S with type t = 'a) -> 'a term

  type _ t =
    | Res : ('a term * 'b t) -> ('a * 'b) t
    | End : unit t

  let ( & ) term rest = Res (term, rest)

  let evaluate : type a.
      a t -> (Luma__id.Id.Resource.t, packed) Hashtbl.t -> (a, Luma__core.Error.error) result =
   fun query store ->
    let rec fetch : type a.
        a t -> (Luma__id.Id.Resource.t, packed) Hashtbl.t -> (a, Luma__core.Error.error) result =
     fun query store ->
      match query with
      | End -> Ok ()
      | Res (Resource (module R), rest) -> (
          match Hashtbl.find_opt store R.type_id with
          | Some packed -> (
              match unpack (module R) packed with
              | Ok result -> (
                  match fetch rest store with Ok rest -> Ok (result, rest) | Error e -> Error e)
              | Error e -> Error e)
          | None ->
              Error
                (Luma__core.Error.resource_not_found
                   (Printf.sprintf "Could not find resource %s" R.name)))
    in
    fetch query store
end
