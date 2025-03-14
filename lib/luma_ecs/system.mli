module Id = Luma__id.Id
module Resource = Luma__resource.Resource

type ('w, 'a) without_resources = {
  filter : Query.Filter.t;
  query : 'a Query.t;
  run : 'w -> (Id.Entity.t * 'a) list -> 'w;
}
(** [without_resources] represents a system that does not require access to any resources. *)

type ('w, 'a, 'b) with_resources = {
  filter : Query.Filter.t;
  query : 'a Query.t;
  resource_query : 'b Resource.Resource_query.t;
  run : 'w -> (Id.Entity.t * 'a) list -> 'b -> 'w;
}
(** [with_resources] represents a system that runs with the resources returned by the
    [resource_query]. *)

(** Type [('w, 'a) t] represents a system that can either operate with or without resources.

    [WithoutResources (sys)] represents a system that operates without any resources.

    [WithResources (sys)] represents a system that operates with resources of type ['b]. *)
type ('w, 'a) t =
  | WithoutResources : ('w, 'a) without_resources -> ('w, 'a) t
  | WithResources : ('w, 'a, 'b) with_resources -> ('w, 'a) t

val make :
  ?filter:Query.Filter.t ->
  'a Query.t ->
  ('w -> (Id.Entity.t * 'a) list -> 'w) ->
  ('w, 'a) without_resources
(** Makes a system with no required resources. *)

val make_with_resources :
  ?filter:Query.Filter.t ->
  'a Query.t ->
  'b Resource.Resource_query.t ->
  ('w -> (Id.Entity.t * 'a) list -> 'b -> 'w) ->
  ('w, 'a, 'b) with_resources
(** Makes a system with resource represented by type ['b]. *)
