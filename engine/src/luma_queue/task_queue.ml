module Complete = struct
  type t = Pack : { apply : unit -> unit } -> t

  let q : t Queue.t = Queue.create ()
  let push x = Queue.push x q

  let apply () =
    while not (Queue.is_empty q) do
      match Queue.pop q with Pack { apply } -> apply ()
    done
end
