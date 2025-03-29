val load : 'b -> (module Asset.S with type t = 'a) -> string -> 'a

module R : Luma__resource.Resource.S with type t = int
