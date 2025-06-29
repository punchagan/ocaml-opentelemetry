(** Runs a signal gatherer server, and prints out every batch of signals
    received to stdout. This can be used to monitor the signals sent by an
    application, e.g., the test executables defined in /tests/bin/emit1*.ml *)
let () =
  let ipv6 =
    match Sys.argv.(1) with
    | "ipv6" -> true
    | _ -> invalid_arg "only the argument 'ipv6' is recognized"
    | exception Invalid_argument _ -> false
  in
  Signal_gatherer.run ~ipv6 ()
