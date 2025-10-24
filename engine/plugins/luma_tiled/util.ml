let fail path msg = Error (Luma__core.Error.io_finalize path msg)

let get_tileset_for_gid (tilesets : Tileset.map_tileset list) gid =
  let rec aux i best = function
    | [] -> best
    | (ts : Tileset.map_tileset) :: rest ->
        let best' = if compare ts.first_gid gid <= 0 then Some (i, ts) else best in
        aux (i + 1) best' rest
  in
  aux 0 None tilesets
