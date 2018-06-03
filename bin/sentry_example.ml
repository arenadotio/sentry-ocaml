open Core_kernel
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

let send_error =
  let spec = Command.Spec.(
    empty
    +> anon ("dsn" %: string)) in
  Command.async_spec ~summary:"Sends an Error.t to Sentry" spec @@
  fun dsn () ->
  let sentry =
    Uri.of_string dsn
    |> Sentry.of_dsn_exn
  in
  Or_error.try_with (fun () ->
    failwith "Test error!")
  |> function
  | Ok _ -> assert false
  | Error e ->
    let%map event_id = Sentry.capture_error sentry e in
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
    let%map event_id = Sentry.capture_exception sentry
                         ~message:"test from OCaml" e in
    printf !"Event created with event_id: %{sexp: string option}"
      (Option.map ~f:Uuidm.to_string event_id)

let send_exn_context =
  let spec = Command.Spec.(
    empty
    +> anon ("dsn" %: string)) in
  Command.async_spec ~summary:"Sends an exception to Sentry using context" spec
  @@ fun dsn () ->
  let sentry =
    Uri.of_string dsn
    |> Sentry.of_dsn_exn
  in
  Sentry.context sentry (fun () ->
    failwith "Test context!")

let () =
  [ "send-message", send_message
  ; "send-error", send_error
  ; "send-exn", send_exn
  ; "send-exn-context", send_exn_context ]
  |> Command.group ~summary:"Test commands for Sentry"
  |> Command.run
