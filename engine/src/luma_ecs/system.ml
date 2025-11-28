open Luma__id

type ('w, 'component) without_resources = {
  uuid : Uuidm.t;
  name : string;
  filter : Query.Component.Filter.t;
  components_query : 'component Query.Component.t;
  run : 'w -> Command.t -> (Id.Entity.t * 'component) list -> 'w;
}

type ('w, 'component, 'resource) with_resources = {
  uuid : Uuidm.t;
  name : string;
  filter : Query.Component.Filter.t;
  components_query : 'component Query.Component.t;
  resources_query : 'resource Query.Resource.t;
  run : 'w -> Command.t -> (Id.Entity.t * 'component) list -> 'resource -> 'w;
}

type ('w, 'a) t =
  | WithoutResources : ('w, 'a) without_resources -> ('w, 'a) t
  | WithResources : ('w, 'a, 'b) with_resources -> ('w, 'a) t

let make
    ?(filter = Query.Component.Filter.Any)
    ~(components : 'a Query.Component.t)
    name
    (run_fn : 'w -> Command.t -> (Id.Entity.t * 'a) list -> 'w) =
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  WithoutResources { uuid; name; filter; components_query = components; run = run_fn }

let make_with_resources
    ?(filter = Query.Component.Filter.Any)
    ~(components : 'a Query.Component.t)
    ~(resources : 'b Query.Resource.t)
    name
    (run_fn : 'w -> Command.t -> (Id.Entity.t * 'a) list -> 'b -> 'w) =
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  WithResources
    { uuid; name; filter; components_query = components; resources_query = resources; run = run_fn }

let name system =
  match system with WithResources sys -> sys.name | WithoutResources sys -> sys.name

let uuid system =
  match system with WithResources sys -> sys.uuid | WithoutResources sys -> sys.uuid
