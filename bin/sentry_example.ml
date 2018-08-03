open Core_kernel
open Async

(* Note: We currently require the Async scheduler to be running *)

let send_message =
  let spec = Command.Spec.(
    empty
    +> anon ("dsn" %: Sentry.Dsn.arg_exn)) in
  Command.async_spec ~summary:"Sends a message to Sentry" spec @@ fun dsn () ->
  Deferred.unit
  >>| fun () ->
  Sentry.capture_message ~dsn "test from OCaml"

let send_error =
  let spec = Command.Spec.(
    empty
    +> anon ("dsn" %: Sentry.Dsn.arg_exn)) in
  Command.async_spec ~summary:"Sends an Error.t to Sentry" spec @@
  fun dsn () ->
  Deferred.unit
  >>| fun () ->
  Or_error.try_with (fun () ->
    failwith "Test error!")
  |> function
  | Ok _ -> assert false
  | Error e ->
    Sentry.capture_error ~dsn e

let send_exn =
  let spec = Command.Spec.(
    empty
    +> anon ("dsn" %: Sentry.Dsn.arg_exn)) in
  Command.async_spec ~summary:"Sends an exception to Sentry" spec @@
  fun dsn () ->
  Deferred.unit
  >>| fun () ->
  try
    failwith "Test exception!"
  with e ->
    Sentry.capture_exception ~dsn ~message:"test from OCaml" e

let send_exn_context =
  let spec = Command.Spec.(
    empty
    +> anon ("dsn" %: Sentry.Dsn.arg_exn)) in
  Command.async_spec ~summary:"Sends an exception to Sentry using context" spec
  @@ fun dsn () ->
  Sentry.context ~dsn (fun () ->
    failwith "Test context!")

let () =
  [ "send-message", send_message
  ; "send-error", send_error
  ; "send-exn", send_exn
  ; "send-exn-context", send_exn_context ]
  |> Command.group ~summary:"Test commands for Sentry"
  |> Command.run
