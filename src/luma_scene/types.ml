open Luma__id
open Luma__ecs
open Luma__resource

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
}
