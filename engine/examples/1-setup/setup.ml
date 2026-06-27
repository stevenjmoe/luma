(* 
   This is the most basic setup for the engine. 
   Running `dune exec examples/1-setup/setup.exe` will open an empty game window.

   dune exec examples/1-setup/setup.exe --profile release
*)

(* Initialise the engine with the desired driver *)
module L = Luma.Make (Luma_driver_raylib.Driver)

let () =
  let open L in
  App.create ()
  (* add_default_plugins adds the minimum required plugins that the engine requires to run. 
     Most people will want to call this. *)
  |> Plugin.add_default_plugins
  |> App.run
