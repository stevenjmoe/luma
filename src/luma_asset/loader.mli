(** A single-variant GADT that packages a dynamically typed asset.

    The pair ([(module A) * value]) represents a first-class module [A] which implements [Asset.S],
    with some hidden type ['a], and a value of the same type ['a]. *)
type loaded = Loaded : (module Asset.S with type t = 'a) * 'a -> loaded

and t = {
  exts : string list;
  load : string -> (loaded, string) result;
}
(** [exts]: A list of the file extensions that this loader supports, without the fullstop.

    [load path load_fn] should accept a path and return [Ok loaded] if the file was successfully
    loaded, otherwise [Error msg] *)

val match_extension : t -> path:string -> bool
