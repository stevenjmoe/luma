open Luma__app
open Luma__ecs
open Luma__resource
open Luma__type_register

let ensure_resource (type a) (module R : Resource.S with type t = a) ~(create : unit -> a) world =
  match World.get_resource world R.type_id with
  | None ->
      let registry = create () in
      let packed = Luma__resource.Resource.pack (module R) registry in
      World.add_resource R.type_id packed world
  | Some _ -> world

let ensure_comp_resource world =
  ensure_resource
    (module Type_register.Component_registry.R)
    ~create:Type_register.Component_registry.create world

let ensure_res_resource world =
  ensure_resource
    (module Type_register.Resource_registry.R)
    ~create:Type_register.Resource_registry.create world

let apply app =
  let w = App.world app in
  w |> ensure_comp_resource |> ensure_res_resource |> ignore;
  app
