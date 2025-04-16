open Luma__ecs

type t = {
  mutable dt : float;
  mutable elapsed : float;
}

let dt t = t.dt

module R = Luma__resource.Resource.Make (struct
  type inner = t
end)

let update_time () =
  System.make
    ~components:Query.(End)
    (fun (world : World.t) _ ->
      match World.get_resource world R.id with
      | Some r -> (
          match Luma__resource.Resource.unpack (module R) r with
          | Ok time ->
              let dt = Raylib.get_frame_time () in
              time.dt <- dt;
              time.elapsed <- time.elapsed +. dt;
              world
          | Error _ -> failwith "could not get time from resources")
      | None -> failwith "could not get time from resources")

let plugin app =
  let open Luma__app in
  let world = App.world app in
  let time = { dt = 0.0016; elapsed = 0. } in
  let packed_time = Luma__resource.Resource.pack (module R) time in
  world |> World.add_resource R.id packed_time |> ignore;
  app |> App.add_system (Update (WithoutResources (update_time ())))
