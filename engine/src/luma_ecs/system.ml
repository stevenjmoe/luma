open Luma__id

type ('w, 'a) without_resources = {
  uuid : Uuidm.t;
  name : string;
  filter : Query.Component.Filter.t;
  components_query : 'a Query.Component.t;
  run : 'w -> (Id.Entity.t * 'a) list -> 'w;
}

type ('w, 'a, 'b) with_resources = {
  uuid : Uuidm.t;
  name : string;
  filter : Query.Component.Filter.t;
  components_query : 'a Query.Component.t;
  resources_query : 'b Query.Resource.t;
  run : 'w -> (Id.Entity.t * 'a) list -> 'b -> 'w;
}

type ('w, 'a) t =
  | WithoutResources : ('w, 'a) without_resources -> ('w, 'a) t
  | WithResources : ('w, 'a, 'b) with_resources -> ('w, 'a) t

let make
    ?(filter = Query.Component.Filter.Any)
    ~(components : 'a Query.Component.t)
    name
    (run_fn : 'w -> (Id.Entity.t * 'a) list -> 'w) =
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  WithoutResources { uuid; name; filter; components_query = components; run = run_fn }

let make_with_resources
    ?(filter = Query.Component.Filter.Any)
    ~(components : 'a Query.Component.t)
    ~(resources : 'b Query.Resource.t)
    name
    (run_fn : 'w -> (Id.Entity.t * 'a) list -> 'b -> 'w) =
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  WithResources
    { uuid; name; filter; components_query = components; resources_query = resources; run = run_fn }

let name system =
  match system with WithResources sys -> sys.name | WithoutResources sys -> sys.name

let uuid system =
  match system with WithResources sys -> sys.uuid | WithoutResources sys -> sys.uuid
