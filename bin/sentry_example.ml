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

let send_exn =
  let spec = Command.Spec.(
    empty
    +> anon ("dsn" %: string)) in
  Command.async_spec ~summary:"Sends an exception to Sentry" spec @@
  fun dsn () ->
  let sentry =
    Uri.of_string dsn
    |> Sentry.of_dsn_exn
  in
  try
    failwith "Test exception!"
  with e ->
    let%map event_id = Sentry.capture_exn sentry ~message:"test from OCaml" e in
    printf !"Event created with event_id: %{sexp: string option}"
      (Option.map ~f:Uuidm.to_string event_id)

let () =
  [ "send-message", send_message
  ; "send-exn", send_exn ]
  |> Command.group ~summary:"Test commands for Sentry"
  |> Command.run
