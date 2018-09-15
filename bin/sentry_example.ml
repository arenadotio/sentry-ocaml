open Core_kernel
open Async

(* Note: We currently require the Async scheduler to be running *)

let send_message =
  let spec = Command.Spec.empty in
  Command.async_spec ~summary:"Sends a message to Sentry" spec @@ fun () ->
  Sentry.merge_tags [ "subcommand", "send-message" ];
  Sentry.(add_breadcrumb (Breadcrumb.make ~message:"first crumb" ()));
  Sentry.(add_breadcrumb (Breadcrumb.make ~message:"second crumb" ()));
  Sentry.capture_message "test from OCaml"
  |> return

let send_exn =
  let spec = Command.Spec.empty in
  Command.async_spec ~summary:"Sends an exception to Sentry" spec
  @@ fun () ->
  Sentry.merge_tags [ "subcommand", "send-exn" ];
  Sentry.(add_breadcrumb (Breadcrumb.make ~message:"first crumb" ()));
  Sentry.(add_breadcrumb (Breadcrumb.make ~message:"second crumb" ()));
  Sentry.with_exn_handler_ignore (fun () ->
    failwith "Test exception!")
  |> return

let () =
  [ "send-message", send_message
  ; "send-exn", send_exn ]
  |> Command.group ~summary:"Test commands for Sentry"
  |> Command.run
