type loaded = Loaded : (module Asset.S with type t = 'a) * 'a -> loaded

type t = {
  exts : string list;
  load : string -> (loaded, string) result;
}

let match_extension t ~path =
  let ext = Filename.extension path in
  List.exists (fun e -> e = ext) t.exts
