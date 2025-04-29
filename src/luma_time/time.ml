open Luma__ecs

module type S = sig
  type t

  val dt : t -> float
  val elapsed : t -> float

  module R : Luma__resource.Resource.S with type t = t

  val update_time : unit -> (World.t, unit) System.without_resources
  val plugin : Luma__app__App.t -> Luma__app__App.t
end

module Make (D : Luma__driver.Driver.S) : S = struct
  type t = {
    mutable dt : float;
    mutable elapsed : float;
  }

  let dt t = t.dt
  let elapsed t = t.elapsed

  module R = Luma__resource.Resource.Make (struct
    type inner = t
  end)

  let log = Luma__core.Log.sub_log "luma.time"

  let update_time () =
    System.make
      ~components:Query.(End)
      (fun (world : World.t) _ ->
        match World.get_resource world R.id with
        | Some r -> (
            match Luma__resource.Resource.unpack (module R) r with
            | Ok time ->
                let dt = D.get_frame_time () in
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
end
