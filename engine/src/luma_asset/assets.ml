type failed = {
  path : string;
  msg : string;
}

type status =
  | Loading
  | Ready of Asset.packed
  | Failed of failed

type asset_record = {
  mutable status : status;
  generation : int;
  type_id : Luma__id.Id.Asset_type.t;
  path : string option;
}

type t = (Luma__id.Id.Asset.t, asset_record) Hashtbl.t

(* TODO: There is no type safety when retrieving handles. You can pass any handle to the get 
   function regardless of the module provided. *)
type handle = {
  id : Luma__id.Id.Asset.t;
  type_id : Luma__id.Id.Asset_type.t;
  generation : int;
  path : string option;
}

let create () = Hashtbl.create 16

let add_pending (type a) (module A : Asset.S with type t = a) ?path assets =
  let id = Luma__id.Id.Asset.next () in
  let record = { status = Loading; generation = 1; type_id = A.type_id; path } in
  Hashtbl.replace assets id record;
  { id; type_id = A.type_id; generation = 1; path }

let add (type a) (module A : Asset.S with type t = a) ?path assets asset =
  let id = Luma__id.Id.Asset.next () in
  let generation = 1 in
  let packed = Asset.pack (module A) asset in
  let record = { status = Ready packed; generation; type_id = A.type_id; path } in
  Hashtbl.replace assets id record;
  { id; type_id = A.type_id; generation; path }

let resolve
    (type a)
    (module A : Asset.S with type t = a)
    (assets : t)
    (h : handle)
    (asset : Asset.packed) : unit =
  match Hashtbl.find_opt assets h.id with
  | Some r when r.generation = h.generation && Luma__id.Id.Asset_type.eq r.type_id A.type_id ->
      r.status <- Ready asset
  | _ -> ()

let fail (assets : t) (h : handle) (failed : failed) : unit =
  match Hashtbl.find_opt assets h.id with
  | Some r when r.generation = h.generation -> r.status <- Failed failed
  | _ -> ()

let get (type a) (module A : Asset.S with type t = a) assets (handle : handle) =
  match Hashtbl.find_opt assets handle.id with
  | None -> None
  | Some (record : asset_record) when record.generation = handle.generation -> (
      match record.status with
      | Ready packed -> (
          match Asset.unpack (module A) packed with Ok asset -> Some asset | Error _ -> None)
      | _ -> None)
  | _ -> None

let get_all (type a) (module A : Asset.S with type t = a) (assets : t) =
  assets
  |> Hashtbl.to_seq
  |> Seq.filter_map (fun (_, (record : asset_record)) ->
         if Luma__id.Id.Asset_type.eq record.type_id A.type_id then
           match record.status with
           | Ready packed -> (
               match Asset.unpack (module A) packed with Ok v -> Some v | Error _ -> None)
           | _ -> None
         else None)
  |> List.of_seq

let exists (assets : t) handle =
  match Hashtbl.find_opt assets handle.id with
  | Some r -> r.generation = handle.generation
  | _ -> false

let is_loaded assets handle =
  match Hashtbl.find_opt assets handle.id with
  | Some { status = Ready _; generation } -> generation = handle.generation
  | _ -> false

let unload (assets : t) handle =
  match Hashtbl.find_opt assets handle.id with
  | None -> ()
  | Some record ->
      if record.generation = handle.generation then Hashtbl.remove assets handle.id else ()

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "Assets"
end)

module For (A : Asset.S) = struct
  type nonrec t = t
  type nonrec handle = handle

  let add ?path assets v = add (module A) ?path assets v
  let add_pending ?path assets = add_pending (module A) ?path assets
  let resolve assets handle packed = resolve (module A) assets handle packed
  let fail = fail
  let exists = exists
  let is_loaded = is_loaded
  let unload = unload
  let get assets h = get (module A) assets h
  let get_all assets = get_all (module A) assets
end
