(** [show_of_pp pp value] takes a pretty-printing function and a value, formats the value using the
    provided function, and returns the resulting string. *)
let show_of_pp pp value =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  pp ppf value;
  Format.pp_print_flush ppf ();
  Buffer.contents buf
