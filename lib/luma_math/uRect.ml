(* TODO: Not unsigned *)
type t = { 
  (** The minimum corner point *)
  min : UVec2.t;
  (** The maximum corner point *)
  max : UVec2.t }

let create min max = { min; max }
