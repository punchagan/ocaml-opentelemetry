module Client = Opentelemetry_client
module Proto = Opentelemetry.Proto
open Clients_e2e_lib

let () =
  Clients_e2e_lib.run_tests
    [
      ( "emit1_eio",
        {
          (* Cohttp_eio tries to use IPv6 first. And if the host supports it but
             the cohttp client does not, we end up with a refused connection. *)
          ipv6 = true;
          jobs = 1;
          iterations = 1;
          batch_traces = 2;
          batch_metrics = 2;
          batch_logs = 2;
        } );
      ( "emit1_eio",
        {
          ipv6 = true;
          jobs = 3;
          iterations = 1;
          batch_traces = 400;
          batch_metrics = 3;
          batch_logs = 400;
        } );
    ]
