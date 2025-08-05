open Luma__id
open Luma__core

module Component = struct
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
    | Query (Required (module C), rest) -> Id.ComponentSet.add C.id (required_ids rest)
    | Query (Optional (module C), rest) -> required_ids rest
    | End -> Id.ComponentSet.empty

  (* TODO: Not too happy with handling with this in an exception but it was the fastest approach. Fix later!*)
  exception Eval_exn of Error.error

  let evaluate : type a.
      ?filter:Filter.t -> a t -> Archetype.t list -> ((Id.Entity.t * a) list, Error.error) result =
   fun ?(filter = Filter.Any) query archetypes ->
    let matches = Filter.matches filter in
    let rec fetch : type a. a t -> Archetype.t -> Id.Entity.t -> a =
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
                   (Error.component_not_found (Id.Component.to_int C.id) (Archetype.hash arch)
                      "Could not evaluate query."))
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
               matches components && Id.ComponentSet.subset required_ids components)
        |> List.concat_map (fun a ->
               Archetype.entities a
               |> Id.EntitySet.to_list
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

  let evaluate : type a. a t -> (Id.Resource.t, packed) Hashtbl.t -> (a, Error.error) result =
   fun query store ->
    let rec fetch : type a. a t -> (Id.Resource.t, packed) Hashtbl.t -> (a, Error.error) result =
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
              Error (Error.resource_not_found (Printf.sprintf "Could not find resource %s" R.name)))
    in
    fetch query store
end

module Tuple = struct
  let with1 (a, _) fn = fn a
  let with2 (a, (b, _)) fn = fn a b
  let with3 (a, (b, (c, _))) fn = fn a b c
  let with4 (a, (b, (c, (d, _)))) fn = fn a b c d
  let with5 (a, (b, (c, (d, (e, _))))) fn = fn a b c d e
  let with6 (a, (b, (c, (d, (e, (f, _)))))) fn = fn a b c d e f
  let with7 (a, (b, (c, (d, (e, (f, (g, _))))))) fn = fn a b c d e f g
  let with8 (a, (b, (c, (d, (e, (f, (g, (h, _)))))))) fn = fn a b c d e f g h
  let with9 (a, (b, (c, (d, (e, (f, (g, (h, (i, _))))))))) fn = fn a b c d e f g h i
  let with10 (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, _)))))))))) fn = fn a b c d e f g h i j
  let iter1 f ents = List.iter (fun (_e, comps) -> with1 comps f) ents
  let iter2 f ents = List.iter (fun (_e, comps) -> with2 comps f) ents
  let iter3 f ents = List.iter (fun (_e, comps) -> with3 comps f) ents
  let iter4 f ents = List.iter (fun (_e, comps) -> with4 comps f) ents
  let iter5 f ents = List.iter (fun (_e, comps) -> with5 comps f) ents
  let iter6 f ents = List.iter (fun (_e, comps) -> with6 comps f) ents
  let iter7 f ents = List.iter (fun (_e, comps) -> with7 comps f) ents
  let iter8 f ents = List.iter (fun (_e, comps) -> with8 comps f) ents
  let iter9 f ents = List.iter (fun (_e, comps) -> with9 comps f) ents
  let iter10 f ents = List.iter (fun (_e, comps) -> with10 comps f) ents

  let iter_e1 f ents =
    let step (e, comps) = with1 comps (f e) in
    List.iter step ents

  let iter_e2 f ents =
    let step (e, comps) = with2 comps (f e) in
    List.iter step ents

  let iter_e3 f ents =
    let step (e, comps) = with3 comps (f e) in
    List.iter step ents

  let iter_e4 f ents =
    let step (e, comps) = with4 comps (f e) in
    List.iter step ents

  let iter_e5 f ents =
    let step (e, comps) = with5 comps (f e) in
    List.iter step ents

  let iter_e6 f ents =
    let step (e, comps) = with6 comps (f e) in
    List.iter step ents

  let iter_e7 f ents =
    let step (e, comps) = with7 comps (f e) in
    List.iter step ents

  let iter_e8 f ents =
    let step (e, comps) = with8 comps (f e) in
    List.iter step ents

  let iter_e9 f ents =
    let step (e, comps) = with9 comps (f e) in
    List.iter step ents

  let iter_e10 f ents =
    let step (e, comps) = with10 comps (f e) in
    List.iter step ents
end
