(* A runs tests against a OTel-instrumented program  *)

module Client = Opentelemetry_client
module Signal = Client.Signal
module Proto = Opentelemetry.Proto
open Lwt.Syntax

(* Server to collect telemetry data *)
module Server = struct
  let dbg_request kind req pp data : unit Lwt.t =
    let _ = kind, req, pp, data in
    (* NOTE: Uncomment for debugging *)
    (* let* () = *)
    (*   let req : string = Format.asprintf "%a" Http.Request.pp req in *)
    (*   let data_s : string = Format.asprintf "%a" pp data in *)
    (*   Lwt_io.fprintf Lwt_io.stderr "# received %s\nREQUEST: %s\nBODY: %s\n@." *)
    (*     kind req data_s *)
    (* in *)
    Lwt.return ()

  let metrics req data =
    let metrics = Signal.Decode.metrics data in
    let+ () = dbg_request "metrics" req Signal.Pp.metrics metrics in
    Signal.Metrics metrics

  let handler push_signal _socket (request : Http.Request.t)
      (body : Cohttp_lwt.Body.t) =
    let* data = Cohttp_lwt.Body.to_string body in
    let* status, signal =
      match Http.Request.resource request with
      | "/v1/traces" ->
        let traces = Signal.Decode.traces data in
        let+ () = dbg_request "trace" request Signal.Pp.traces traces in
        `OK, Some (Signal.Traces traces)
      | "/v1/metrics" ->
        let metrics = Signal.Decode.metrics data in
        let+ () = dbg_request "metrics" request Signal.Pp.metrics metrics in
        `OK, Some (Signal.Metrics metrics)
      | "/v1/logs" ->
        let logs = Signal.Decode.logs data in
        let+ () = dbg_request "logs" request Signal.Pp.logs logs in
        `OK, Some (Signal.Logs logs)
      | unexepected ->
        let+ () =
          Lwt_io.eprintf "unexpected endpoint %s -- status %s\n" unexepected
            (Http.Status.to_string `Not_found)
        in
        `Not_found, None
    in
    push_signal signal;
    let resp_body = Cohttp_lwt.Body.of_string "" in
    Cohttp_lwt_unix.Server.respond ~status ~body:resp_body ()

  let run ~ipv6 port push_signals =
    (* FIXME: Ideally we could bind both IPv6 and IPv4, and not have to worry
       about manually deciding which to use. However, Cohttp depends on Conduit
       for this, which is not currently able to support a dual stack. See
       https://github.com/mirage/ocaml-conduit/issues/323 . When that is
       resolved, we should be able to remove this logic, along with all
       ccurences of the [ipv6] argument. *)
    let* ctx =
      if ipv6 then
        let+ ctx = Conduit_lwt_unix.init ~src:"::1" () in
        Some (Cohttp_lwt_unix.Net.init ~ctx ())
      else
        Lwt.return None
    in
    let* () = Lwt_io.eprintf "starting server on http://localhost:%d\n" port in
    Cohttp_lwt_unix.Server.(
      make ~callback:(handler push_signals) ()
      |> create ?ctx ~mode:(`TCP (`Port port)))
end

(** Manage launching and cleaning up the program we are testing *)
module Tested_program = struct
  let validate_exit = function
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED bad_code ->
      failwith
      @@ Printf.sprintf "process under test ended with bad exit code %d"
           bad_code
    | Unix.WSIGNALED i ->
      failwith
      @@ Printf.sprintf "process under test ended with unexpected signal %d" i
    | Unix.WSTOPPED i ->
      failwith
      @@ Printf.sprintf "process under test ended with unexpected stop %d" i

  let run program_to_test =
    let redirect = `FD_copy Unix.stderr in
    let cmd = "", Array.of_list program_to_test in
    (* Give server time to be online *)
    let* () = Lwt_unix.sleep 0.5 in
    let* () =
      Lwt_io.eprintf "running command: %s\n"
        (Format.asprintf "%a"
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " ")
              Format.pp_print_string)
           program_to_test)
    in
    let* result = Lwt_process.exec ~stdout:redirect cmd in
    (* Give server time process signals *)
    let+ () = Lwt_unix.sleep 0.5 in
    validate_exit result
end

let collect_traces ~ipv6 ~port program_to_test push_signals () =
  let* () =
    Lwt.pick
      [ Server.run ~ipv6 port push_signals; Tested_program.run program_to_test ]
  in
  (* Let the tester know all the signals have be sent *)
  Lwt.return (push_signals None)

let default_port =
  String.split_on_char ':' Client.Config.default_url |> function
  (* Extracting the port from 'http://foo:<port>' *)
  | [ _; _; port ] -> int_of_string port
  | _ -> failwith "unexpected format in Client.Config.default_url"

let gather_signals ~ipv6 ?(port = default_port) program_to_test =
  Lwt_main.run
  @@
  let stream, push = Lwt_stream.create () in
  let* () = collect_traces ~ipv6 ~port program_to_test push () in
  Lwt_stream.to_list stream

(* Just run the server, and print the signals gathered. *)
let run ?(port = default_port) ~ipv6 () =
  Lwt_main.run
  @@
  let stream, push = Lwt_stream.create () in
  Lwt.join
    [
      Server.run ~ipv6 port push;
      Lwt_stream.iter_s
        (fun s -> Format.asprintf "%a" Signal.Pp.pp s |> Lwt_io.printl)
        stream;
    ]
