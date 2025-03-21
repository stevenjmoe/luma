module Resource = Luma__resource.Resource

type load_state = Loading | Loaded of Asset.asset | Failed of string

(*TODO: Support more efficient storage options. *)
(* TODO: this shouldn't have a string key. Fix. *)
type t = { hash_tbl : (string, load_state) Hashtbl.t }

let create () = { hash_tbl = Hashtbl.create 16 }
let insert t id asset = Hashtbl.replace t.hash_tbl id asset
let get t id = Hashtbl.find_opt t.hash_tbl id
let contains t id = match Hashtbl.find_opt t.hash_tbl id with Some _ -> true | None -> false

module R = Resource.Make (struct
  type inner = t
end)
