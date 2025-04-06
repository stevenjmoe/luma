type t = { exts : string list; load : string -> (Asset.packed, string) result }

let match_extension t ~path =
  let ext = Filename.extension path in
  List.exists (fun e -> e = ext) t.exts
