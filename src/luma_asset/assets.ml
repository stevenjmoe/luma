type t = (Luma__id.Id.Asset.t, Asset.packed list) Hashtbl.t

let create () = Hashtbl.create 16

let add (type a) t (module A : Asset.S with type t = a) (value : A.t) =
  let current = Hashtbl.find_opt t A.id |> Option.value ~default:[] in
  Hashtbl.replace t A.id (Asset.pack (module A) value :: current)

let get_all (type a) t (module A : Asset.S with type t = a) =
  match Hashtbl.find_opt t A.id with
  | None -> []
  | Some packed_list ->
      List.fold_left
        (fun acc packed ->
          match Asset.unpack (module A) packed with Ok a -> a :: acc | Error _ -> acc)
        [] packed_list

let exists t (module A : Asset.S) = Hashtbl.mem t A.id

(* Provided assets *)
(* TODO:*)
(*module Texture_atlas = struct
  type t = Luma__image.Image.Texture_atlas.t

  module R = Asset.Make (struct
    type inner = t

    let file_extensions = [ ".png"; ".jpg" ]
    let decode path = Raylib.load_image
  end)
end*)

module Texture = struct
  type t = Luma__texture.Texture.t

  module R = Asset.Make (struct
    type inner = t

    let file_extensions = [ ".png"; ".jpg" ]
    let decode path = Luma__texture.Texture.load path
  end)
end

module R = Luma__resource.Resource.Make (struct
  type inner = t
end)
