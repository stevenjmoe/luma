module Event = struct
  type t =
    | File_read_ok of {
        id : int;
        path : string;
        bytes : bytes;
      }
    | File_read_err of {
        id : int;
        path : string;
        err : Error.error;
      }
end
