(** Stubs for drivers that don't support 3D. [include] this inside a driver. *)

exception Unsupported of string

type camera3d = unit
type mesh = unit

module Camera3d = struct
  let perspective ~position:_ ~target:_ ~up:_ ~fov_y_degrees:_ () =
    raise (Unsupported "Camera3d.perspective")
end

module Mesh3d = struct
  let create _data = raise (Unsupported "Mesh3d.create")
  let unload _mesh = raise (Unsupported "Mesh3d.unload")
end

module Draw3d = struct
    let with_camera _camera _draw = raise (Unsupported "Draw3d.with_camera")
    let cube ~center:_ ~size:_ _colour = raise (Unsupported "Draw3d.cube")
    let cube_wires ~center:_ ~size:_ _colour = raise (Unsupported "Draw3d.cube_wires")
    let grid ~slices:_ ~spacing:_ = raise (Unsupported "Draw3d.grid")
    let mesh _mesh ~position:_ ~scale:_ = raise (Unsupported "Draw3d.mesh")
end
