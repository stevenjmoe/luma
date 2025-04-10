type asset_record = {
  packed : Asset.packed;
  generation : int;
  type_id : Luma__id.Id.Asset_type.t;
}

type t = (Luma__id.Id.Asset.t, asset_record) Hashtbl.t

type 'a handle = {
  id : Luma__id.Id.Asset.t;
  type_id : Luma__id.Id.Asset_type.t;
  generation : int;
}

let create () = Hashtbl.create 16

let add (type a) (module A : Asset.S with type t = a) assets ~id ~generation ~asset =
  let packed = Asset.pack (module A) asset in
  let record = { packed; generation; type_id = A.type_id } in
  Hashtbl.replace assets id record;
  { id; type_id = A.type_id; generation }

let get (type a) (module A : Asset.S with type t = a) assets (handle : a handle) =
  match Hashtbl.find_opt assets handle.id with
  | None -> None
  | Some (record : asset_record) ->
      if record.generation = handle.generation then
        match Asset.unpack (module A) record.packed with
        | Ok asset -> Some asset
        | Error _ -> None
      else
        None

let unload (assets : t) handle =
  match Hashtbl.find_opt assets handle.id with
  | None -> ()
  | Some record ->
      if record.generation = handle.generation then
        Hashtbl.remove assets handle.id
      else
        ()

module R = Luma__resource.Resource.Make (struct
  type inner = t
end)
