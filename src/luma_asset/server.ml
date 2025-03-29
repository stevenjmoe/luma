let load (type a) assets (module A : Asset.S with type t = a) path =
  let ext = Filename.extension path in
  if not (List.mem ext A.file_extensions) then
    failwith (Printf.sprintf "Failed to load asset: extension %s not supported" ext)
  else
    A.decode path

module R = Luma__resource.Resource.Make (struct
  type inner = int
end)
