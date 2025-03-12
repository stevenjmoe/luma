type ('w, 'a) without_resource = {
  filter : Query.Filter.t;
  query : 'a Query.t;
  run : 'w -> (Id.Entity.t * 'a) list -> 'w;
}

type ('w, 'a, 'b) with_resource = {
  filter : Query.Filter.t;
  query : 'a Query.t;
  resource_query : 'b Resource.Resource_query.t;
  run : 'w -> (Id.Entity.t * 'a) list -> 'b -> 'w;
}

type ('w, 'a) t =
  | WithoutResources : ('w, 'a) without_resource -> ('w, 'a) t
  | WithResources : ('w, 'a, 'b) with_resource -> ('w, 'a) t

let make
    ?(filter = Query.Filter.Any)
    (query : 'a Query.t)
    (run_fn : 'w -> (Id.Entity.t * 'a) list -> 'w) : ('w, 'a) without_resource =
  { filter; query; run = run_fn }

let make_with_resources
    ?(filter = Query.Filter.Any)
    (query : 'a Query.t)
    (resource_query : 'b Resource.Resource_query.t)
    (run_fn : 'w -> (Id.Entity.t * 'a) list -> 'b -> 'w) : ('w, 'a, 'b) with_resource =
  { filter; query; resource_query; run = run_fn }
