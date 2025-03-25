(* TODO: It would be nice to not have to separate resources from no resources *)

type ('w, 'a) without_resources = {
  filter : Query.Filter.t;
  query : 'a Query.t;
  run : 'w -> (Luma__id.Id.Entity.t * 'a) list -> 'w;
}

type ('w, 'a, 'b) with_resources = {
  filter : Query.Filter.t;
  query : 'a Query.t;
  resource_query : 'b Luma__resource.Resource.Query.t;
  run : 'w -> (Luma__id.Id.Entity.t * 'a) list -> 'b -> 'w;
}

type ('w, 'a) t =
  | WithoutResources : ('w, 'a) without_resources -> ('w, 'a) t
  | WithResources : ('w, 'a, 'b) with_resources -> ('w, 'a) t

let make
    ?(filter = Query.Filter.Any)
    (query : 'a Query.t)
    (run_fn : 'w -> (Luma__id.Id.Entity.t * 'a) list -> 'w) : ('w, 'a) without_resources =
  { filter; query; run = run_fn }

let make_with_resources
    ?(filter = Query.Filter.Any)
    (query : 'a Query.t)
    (resource_query : 'b Luma__resource.Resource.Query.t)
    (run_fn : 'w -> (Luma__id.Id.Entity.t * 'a) list -> 'b -> 'w) : ('w, 'a, 'b) with_resources =
  { filter; query; resource_query; run = run_fn }
