type asset_record = { packed : Asset.packed; generation : int }
type t = (Luma__id.Id.Asset.t, asset_record) Hashtbl.t

let create () = Hashtbl.create 16

type handle = { id : Luma__id.Id.Asset.t; generation : int }

let add assets ~id ~packed ~generation =
  let record = { packed; generation } in
  Hashtbl.replace assets id record

let get (type a) (module A : Asset.S with type t = a) (assets : t) handle =
  match Hashtbl.find_opt assets handle.id with
  | None -> None
  | Some record ->
      if record.generation = handle.generation then
        match Asset.unpack (module A) record.packed with
        | Ok asset -> Some asset
        | Error _ -> failwith ""
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

(* Provided assets *)
module Texture_atlas = struct
  type t = Luma__image.Image.Texture_atlas.t

  module R = Asset.Make (struct
    type inner = t
  end)
end

module Texture = struct
  type t = Raylib.Texture.t

  module A = Asset.Make (struct
    type inner = t
  end)
end

module R = Luma__resource.Resource.Make (struct
  type inner = t
end)
