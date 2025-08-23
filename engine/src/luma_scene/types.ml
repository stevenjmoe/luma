open Luma__asset
open Luma__id
open Luma__ecs
open Luma__resource

type meta = {
  version : int;
  created_at : float;
  engine_rev : string;
}

type entity = {
  uuid : Uuidm.t;
  name : string;
  components : Component.packed list;
}

type t = {
  id : Id.Scene.t;
  uuid : Uuidm.t;
  name : string;
  entities : entity list;
  resources : Resource.packed list;
  version : int;
}

module A = Asset.Make (struct
  type inner = t
end)
