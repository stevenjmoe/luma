open Luma__id

type t = {
  id : Id.Entity.t;
  uuid : Uuidm.t;
  name : string;
}

let make ?(uuid = None) name =
  let id = Id.Entity.next () in
  let uuid =
    if Option.is_some uuid then Option.get uuid
    else Uuidm.v4_gen (Random.State.make_self_init ()) ()
  in
  { id; uuid; name }

let id e = e.id
let uuid e = e.uuid
let name e = e.name
