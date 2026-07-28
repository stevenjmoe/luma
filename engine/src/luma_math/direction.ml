open Constants

module Dir2 = struct
  type t = Vec2.t

  type invalid_direction_error =
    | Zero
    | Infinite
    | NaN

  type error = [ `Invalid_direction of invalid_direction_error ]

  let invalid_direction_error_of_length length =
    if Float.is_nan length then NaN else if not (Float.is_finite length) then Infinite else Zero

  let create_and_length v : (Vec2.t * float, [> error ]) result =
    let len = Vec2.length v in
    if Float.is_finite len && len > 0. then
      let inv = 1.0 /. len in
      let dir = Vec2.scale inv v in
      Ok (dir, len)
    else Error (`Invalid_direction (invalid_direction_error_of_length len))

  let create v = Result.map fst (create_and_length v)

  let create_unchecked v =
    let length_squared = Vec2.length_squared v in
    assert (Float.is_finite length_squared && Float.abs (length_squared -. 1.0) <= 2e-2);
    v

  let of_xy x y = create (Vec2.create x y)
  let of_xy_unchecked x y = create_unchecked (Vec2.create x y)

  (** A vector which points along the positive x axis. *)
  let pos_x = create_unchecked Vec2.pos_x

  (** A vector which points along the positive y axis. *)
  let pos_y = create_unchecked Vec2.pos_y

  (** A vector which points along the negative x axis. *)
  let neg_x = create_unchecked Vec2.neg_x

  (** A vector which points along the negative y axis. *)
  let neg_y = create_unchecked Vec2.neg_y

  (** Equivalent to [pos_y]. *)
  let north = create_unchecked Vec2.pos_y

  (** Equivalent to [neg_y]. *)
  let south = create_unchecked Vec2.neg_y

  (** Equivalent to [pos_x]. *)
  let east = create_unchecked Vec2.pos_x

  (** Equivalent to [neg_x]. *)
  let west = create_unchecked Vec2.neg_x

  (** Between [north] and [east]. *)
  let north_east = create_unchecked (Vec2.create frac_1_sqrt_2 frac_1_sqrt_2)

  (** Between [north] and [west]. *)
  let north_west = create_unchecked (Vec2.create (-.frac_1_sqrt_2) frac_1_sqrt_2)

  (** Between [south] and [east]. *)
  let south_east = create_unchecked (Vec2.create frac_1_sqrt_2 (-.frac_1_sqrt_2))

  (** Between [south] and [west]. *)
  let south_west = create_unchecked (Vec2.create (-.frac_1_sqrt_2) (-.frac_1_sqrt_2))
end
