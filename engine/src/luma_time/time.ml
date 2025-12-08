open Luma__ecs
open Luma__serialize
open Luma__core
open Luma__app

module type PLUGIN = sig
  type t

  val update_time : unit -> (World.t, unit) System.t
  val plugin : App.t -> App.t
end

type t = {
  mutable dt : float;
  mutable elapsed : float;
}

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "Time"
end)

let dt t = t.dt
let elapsed t = t.elapsed
let log = Luma__core.Log.sub_log "luma.time"

module Plugin (D : Luma__driver.Driver.S) : PLUGIN with type t = t = struct
  type nonrec t = t

  let update_time () =
    let open Luma__id in
    System.make ~components:End "update_time" (fun (world : World.t) _ _ ->
        match World.get_resource world R.type_id with
        | Some r -> (
            match Luma__resource.Resource.unpack (module R) r with
            | Ok time ->
                let dt = D.get_frame_time () in
                time.dt <- dt;
                time.elapsed <- time.elapsed +. dt;
                world
            | Error e ->
                log.error (fun log -> log "Error updating time: %a" Luma__core.Error.pp e);
                world)
        | None ->
            log.error (fun log -> log "error");
            world)

  (*module Time_json_serializer =
    Serialize.Make_serializer
      (Serialize.Json_format)
      (struct
        open Json_helpers

        let normalize s = s |> String.trim |> String.lowercase_ascii

        type nonrec t = t

        let to_repr r : Yojson.Safe.t =
          `Assoc [ (R.name, `Assoc [ ("elapsed", `Float r.elapsed) ]) ]

        let of_repr repr =
          let ( let* ) = Result.bind in

          match repr with
          | `Assoc [ (name, data) ] when normalize name = normalize R.name ->
              let* elapsed = parse_float "elapsed" data in
              Ok { dt = 0.0016; elapsed }
          | _ -> Error (Error.parse_json (Json (Yojson.Safe.pretty_to_string repr)))
      end)*)

  let plugin app =
    let open Luma__app in
    let world = App.world app in
    let time = { dt = 0.0016; elapsed = 0. } in
    let packed_time = Luma__resource.Resource.pack (module R) time in
    (*let serializer = Serialize.pack_json (module Time_json_serializer) in*)

    world |> World.add_resource R.type_id packed_time |> ignore;
    app
    (*|> App.register_resource R.name (module R) [ serializer ]*)
    |> App.on Scheduler.Update (update_time ())
end
