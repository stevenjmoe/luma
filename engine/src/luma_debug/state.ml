open Luma__serialize
open Luma__id
open Luma__resource
open Luma__app

type mouse_debug_mode =
  | Screen
  | World
  | Both
  | Off

type t = {
  mutable open_ : bool;
  mutable filter : string;
  mutable page : int;
  mutable per_page : int;
  mutable cached_rev : int;
  mutable cached_entities : Id.Entity.t list;
  mutable x : float;
  mutable y : float;
  mutable w : float;
  mutable h : float;
  mutable full : bool;
  mutable resizing : bool;
  mutable mouse_debug_mode : mouse_debug_mode;
}

let default () =
  {
    open_ = false;
    filter = "";
    page = 0;
    per_page = 32;
    cached_rev = -1;
    cached_entities = [];
    x = 10.;
    y = 10.;
    w = 200.;
    h = 200.;
    full = false;
    resizing = false;
    mouse_debug_mode = Both;
  }

module R = Resource.Make (struct
  type inner = t

  let name = "debug_state"
  let default = default ()
end)
