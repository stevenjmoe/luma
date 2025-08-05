type loaded = Loaded : (module Asset.S with type t = 'a) * 'a -> loaded

type t = {
  type_id : Luma__id.Id.Asset_type.t;
  exts : string list;
  load : string -> (loaded, string) result;
}

let match_extension (type a) (module A : Asset.S with type t = a) t ~path =
  let ext = Filename.extension path in
  List.exists (fun e -> e = ext && t.type_id = A.type_id) t.exts
