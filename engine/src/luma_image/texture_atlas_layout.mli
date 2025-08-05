open Luma__math

type t
(** A [Texture_atlas_layout] represents the layout of a texture atlas, which is a collection of
    smaller textures arranged in a grid. It contains:
    - [size]: The total size of the atlas.
    - [textures]: A vector of rectangles ([URect.t]) defining the areas within the atlas where each
      texture can be found. *)

val empty : unit -> t
(** [empty ()] creates a new [Texture_atlas_layout] with:
    - [size] set to [UVec2.zero] (indicating no size).
    - [textures] as an empty vector. *)

val size : t -> Vec2.t
(** [size t] returns the total size of the texture atlas as a [UVec2.t]. The size is calculated
    based on the grid dimensions, tile size, padding, and offset. *)

val textures : t -> (Rect.t, Containers.Vector.rw) Containers.Vector.t
(** [textures t] returns a vector of rectangles ([URect.t]) representing the areas within the atlas
    where each texture is located. Each rectangle defines the [min] and [max] coordinates of a
    texture within the atlas. *)

val frame_size : t -> Vec2.t option
(** [frame_size t] returns the size of an individual frame. *)

val last_index : t -> int option
(** [last_index t] returns [None] if the vector is empty, otherwise the index of the last inserted
    texture. *)

val from_grid : ?padding:Vec2.t -> ?offset:Vec2.t -> Vec2.t -> int -> int -> t
(** [from_grid ~padding ~offset tile_size columns rows] creates a [Texture_atlas_layout] for a
    grid-based texture atlas.
    - [padding]: Optional padding between tiles (default: [UVec2.zero]).
    - [offset]: Optional offset for the entire grid (default: [UVec2.zero]).
    - [tile_size]: The size of each texture as a [UVec2.t].
    - [columns]: The number of columns in the grid.
    - [rows]: The number of rows in the grid.

    The function calculates the positions of all tiles within the atlas and stores them in the
    [textures] vector. It also computes the total [size] of the atlas. *)

val length : t -> int
(** [length t] returns the number of textures in the atlas. *)

val is_empty : t -> bool
(** [is_empty t] returns [true] if the atlas contains no textures, and [false] otherwise. *)

val get_frame : t -> int -> Rect.t option
(** [get_frame t index] retrievs a frame from [t.textures] at the given index or None if the index
    is out of bounds. *)

val append_texture : Rect.t -> t -> t
(** [add_texture texture  t] appends [texture] to the end of layout [t] and updates [last_index] to
    reflect the new last position.

    @return the updated layout [t]. *)

val insert_texture : Rect.t -> int -> t -> t
(** [insert_texture texture idx t] inserts [texture] into layout [t] at position [idx], shifting
    subsequent elements to the right and updates [last_index] to reflect the new last position.

    @raise Invalid_arg if [idx < 0] or [idx > length t.textures].

    @return the updated layout [t]. *)
