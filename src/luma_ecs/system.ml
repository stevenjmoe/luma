(* TODO: It would be nice to not have to separate resources from no resources *)

type ('w, 'a) without_resources = {
  filter : Query.Filter.t;
  components_query : 'a Query.t;
  run : 'w -> (Luma__id.Id.Entity.t * 'a) list -> 'w;
}

type ('w, 'a, 'b) with_resources = {
  filter : Query.Filter.t;
  components_query : 'a Query.t;
  resources_query : 'b Luma__resource.Resource.Query.t;
  run : 'w -> (Luma__id.Id.Entity.t * 'a) list -> 'b -> 'w;
}

type ('w, 'a) t =
  | WithoutResources : ('w, 'a) without_resources -> ('w, 'a) t
  | WithResources : ('w, 'a, 'b) with_resources -> ('w, 'a) t

let make
    ?(filter = Query.Filter.Any)
    ~(components : 'a Query.t)
    (run_fn : 'w -> (Luma__id.Id.Entity.t * 'a) list -> 'w) : ('w, 'a) without_resources =
  { filter; components_query = components; run = run_fn }

let make_with_resources
    ?(filter = Query.Filter.Any)
    ~(components : 'a Query.t)
    ~(resources : 'b Luma__resource.Resource.Query.t)
    (run_fn : 'w -> (Luma__id.Id.Entity.t * 'a) list -> 'b -> 'w) : ('w, 'a, 'b) with_resources =
  { filter; components_query = components; resources_query = resources; run = run_fn }
