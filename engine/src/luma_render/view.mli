(** Resolved render view.

    A view is the render-time pairing of:
    - a camera entity
    - its camera component
    - a concrete viewport in window space

    The viewport defaults to fullscreen if absent from the camera component.

    Views are resolved every frame and drive rendering. Multiple views may exist (eg. split-screen).
*)

open Luma__camera
open Luma__resource

type t

val create : Luma__id__Id.Entity.t -> Camera.t -> Viewport.t -> t
val camera_entity : t -> Luma__id__Id.Entity.t
val camera : t -> Camera.t
val viewport : t -> Viewport.t

module R : Resource.S with type t = t list
