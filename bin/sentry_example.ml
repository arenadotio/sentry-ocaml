open Core
open Async

let send_message =
  let spec = Command.Spec.(
      empty
      +> anon ("dsn" %: string)) in
  Command.async_spec ~summary:"Sends a message to Sentry" spec @@ fun dsn () ->
  let sentry =
    Uri.of_string dsn
    |> Sentry.of_dsn_exn
  in
  let%map event_id = Sentry.capture_message sentry "test from OCaml" in
  printf !"Event created with event_id: %{sexp: string option}"
    (Option.map ~f:Uuidm.to_string event_id)

let () =
  Command.run send_message
