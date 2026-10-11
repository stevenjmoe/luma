(** Define concrete types for rendering related types that can be shared by every driver. *)

module Mesh_data = struct
    (** CPU-side mesh data. Drivers upload this with [Mesh3d.create]. *)
    type t = {
      positions : float array;
      normals : float array;
      uvs : float array;
      indices : int array option; (* triangles. may be None for non-indexed meshes *)
    }
end
