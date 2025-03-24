(*TODO: Support more efficient storage options. *)
(* TODO: this shouldn't have a string key. Fix. *)
type t = { hash_tbl : (string, string) Hashtbl.t }

let create () = { hash_tbl = Hashtbl.create 16 }
let insert t id asset = Hashtbl.replace t.hash_tbl id asset
let get t id = Hashtbl.find_opt t.hash_tbl id
let contains t id = match Hashtbl.find_opt t.hash_tbl id with Some _ -> true | None -> false

module R = Luma__resource.Resource.Make (struct
  type inner = t
end)
