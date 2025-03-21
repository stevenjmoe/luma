module Asset_server_data = struct
  type t = int
end

type t = { data : Asset_server_data.t }

let create () = { data = 1 }

(* TODO:  should accept an asset type so that assets of that type can be added *)
(*let register_asset : type a. (module S with type t = a) -> a -> unit =
   fun (module A : S with type t = a) asset_data -> ()

  (* TODO:  should accept a file path, load the item from that path if it exists, and return a handle (eventually) *)
  let load t (module A : S) path =
    (* Attempt #1 - Using a preemptive thread to detach the load process. The number of threads is capped at 4 by default.
       This can be changed but I want to experiment with Lwt.async and run a sort of background worker thread. *)
    let task =
      Lwt_preemptive.detach
        (fun _ ->
          Printf.printf "loading asset with path %s.\n%!" path;
          Unix.sleep 3;
          Raylib.load_texture path)
        ()
    in
    let _ =
      Lwt.on_success task (fun _ ->
          print_endline "done";
          ())
    in
    print_endline "loaded";
    ()

  *)
module R = Luma__resource.Resource.Make (struct
  type inner = t
end)
