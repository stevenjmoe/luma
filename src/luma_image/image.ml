open Containers
open Luma__math

module Texture_atlas_layout : sig
  type t
  (** A [Texture_atlas_layout] represents the layout of a texture atlas, which is a collection of
      smaller textures arranged in a grid. It contains:
      - [size]: The total size of the atlas.
      - [textures]: A vector of rectangles ([URect.t]) defining the areas within the atlas where
        each texture can be found. *)

  val empty : unit -> t
  (** [empty ()] creates a new [Texture_atlas_layout] with:
      - [size] set to [UVec2.zero] (indicating no size).
      - [textures] as an empty vector. *)

  val size : t -> Vec2.t
  (** [size t] returns the total size of the texture atlas as a [UVec2.t]. The size is calculated
      based on the grid dimensions, tile size, padding, and offset. *)

  val textures : t -> (Rect.t, Vector.rw) Vector.t
  (** [textures t] returns a vector of rectangles ([URect.t]) representing the areas within the
      atlas where each texture is located. Each rectangle defines the [min] and [max] coordinates of
      a texture within the atlas. *)

  val frame_size : t -> Vec2.t option
  (** [frame_size t] returns the size of an individual frame. *)

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

  val add_texture : t -> Rect.t -> int
  (** [add_texture t texture] Adds a texture to the layout and returns its index. *)
end = struct
  type t = {
    size : Vec2.t;
    textures : (Rect.t, Vector.rw) Vector.t;
    frame_size : Vec2.t option;
  }

  let empty () = { size = Vec2.zero (); textures = Vector.create (); frame_size = None }
  let size t = t.size
  let textures t = t.textures
  let frame_size t = t.frame_size

  let from_grid ?(padding = Vec2.zero ()) ?(offset = Vec2.zero ()) tile_size columns rows =
    let open Vec2.Infix in
    let sprites = Vector.create () in
    let current_padding = Vec2.zero () in
    let foi = Float.of_int in

    let rec loop_rows y =
      if y > 1 then
        current_padding.y <- padding.y;

      let rec loop_cols x =
        if x > 1 then
          current_padding.x <- padding.x;

        let cell = Vec2.create (foi x) (foi y) in

        let rect_min = (tile_size *.. cell) +.. offset in
        Vector.push sprites Rect.{ min = rect_min; max = rect_min +.. tile_size };

        if x < columns then
          loop_cols (x + 1)
        else
          ()
      in
      loop_cols 1;

      if y < rows then
        loop_rows (y + 1)
      else
        ()
    in

    loop_rows 1;
    let grid_size = Vec2.create (foi columns) (foi rows) in
    {
      size = ((tile_size +.. current_padding) *.. grid_size) -.. current_padding;
      textures = sprites;
      frame_size = Some tile_size;
    }

  let length t = Vector.length t.textures
  let is_empty t = Vector.length t.textures = 0

  let get_frame t index =
    if index >= 0 && index < Vector.length t.textures then
      Some (Vector.get t.textures index)
    else
      None

  let add_texture t texture =
    Vector.push t.textures texture;
    Vector.length t.textures - 1
end

module Texture_atlas : sig
  module A : Luma__asset.Asset.S

  type t = A.t

  val from_layout : Texture_atlas_layout.t -> t
  val get_frame : t -> int -> Rect.t option
  val frame_size : t -> Vec2.t option
end = struct
  (* A hidden record type so we can pass it into Asset.Make,
   which produces a nominal type A.t. We then alias t = A.t
   without leaking raw’s internal fields in the public API. *)
  type raw = {
    layout : Texture_atlas_layout.t;
    index : int;
  }

  module A = Luma__asset.Asset.Make (struct
    type inner = raw
  end)

  type t = A.t

  let from_layout layout = { layout; index = 0 }
  let get_frame t index = Texture_atlas_layout.get_frame t.layout index
  let frame_size t = Texture_atlas_layout.frame_size t.layout
end

module Texture = struct
  type t = Raylib.Texture.t

  module A = Luma__asset.Asset.Make (struct
    type inner = t
  end)
end

let register_loader (server : Luma__asset.Server.t) =
  let image_loader =
    {
      Luma__asset.Loader.exts = [ ".png"; ".jpg" ];
      load =
        (fun path ->
          let image = Raylib.load_image path in
          let texture = Raylib.load_texture_from_image image in
          Ok (Loaded ((module Texture.A), texture)));
    }
  in
  Luma__asset.Server.register_loader server image_loader

let () =
  Luma__asset.Server.register_loader_hook (fun server ->
      Luma__asset.Server.register_loader server
        {
          Luma__asset.Loader.exts = [ ".png"; ".jpg" ];
          load =
            (fun path ->
              let image = Raylib.load_image path in
              let texture = Raylib.load_texture_from_image image in
              Ok (Loaded ((module Texture.A), texture)));
        })
