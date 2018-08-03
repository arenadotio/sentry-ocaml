open Core_kernel
open Async_kernel
open Async_unix

module Client = Client
module Config = Config
module Dsn = Dsn
module Event = Event
module Exception = Exception
module Platform = Platform
module Sdk = Sdk
module Severity_level = Severity_level

let dsn_key = Univ_map.Key.create ~name:"dsn" [%sexp_of: Dsn.t]
let environment_key = Univ_map.Key.create ~name:"environment" [%sexp_of: string]

let with_config_item key value f =
  Scheduler.with_local key (Some value) ~f

let with_dsn value f = with_config_item dsn_key value f

let default_environment = Sys.getenv "SENTRY_ENVIRONMENT"

let with_environment value f = with_config_item environment_key value f

let find_config key default =
  Option.first_some (Scheduler.find_local key) default

let capture_event ?exn ?message () =
  let dsn =
    Scheduler.find_local dsn_key
    |> Option.value ~default:Dsn.default
  in
  match dsn with
  | Some dsn ->
    let event =
      let environment =
        find_config environment_key default_environment in
      let exn = Option.map exn ~f:List.return in
      Event.make ?exn ?message ?environment ()
    in
    Log.Global.info "Uploading sentry event %s" (Uuid.unwrap event.event_id);
    Client.send_event ~dsn event
  | _ ->
    Log.Global.info "Not uploading Sentry event because no DSN is set"

let capture_message message =
  let message = Message.make ~message () in
  capture_event ~message ()

let capture_exception ?message exn =
  let exn = Exception.of_exn exn in
  let message =
    Option.map message ~f:(fun message ->
      Message.make ~message ())
  in
  capture_event ?message ~exn ()

let capture_error err =
  let exn = Exception.of_error err in
  capture_event ~exn ()

let context f =
  try
    f ()
  with e ->
    let backtrace = Caml.Printexc.get_raw_backtrace () in
    capture_exception e;
    Caml.Printexc.raise_with_backtrace e backtrace

let context_ignore f =
  try
    f ()
  with e ->
    capture_exception e

let capture_and_return_or_error v =
  match v with
  | Ok _ -> v
  | Error e ->
    capture_error e;
    v

let context_or_error f =
  context (fun () ->
    f ()
    |> capture_and_return_or_error)

let context_async f =
  Monitor.try_with ~extract_exn:false ~rest:(`Call (capture_exception)) f
  >>= function
  | Ok res -> return res
  | Error e ->
    let backtrace = Caml.Printexc.get_raw_backtrace () in
    capture_exception e;
    Caml.Printexc.raise_with_backtrace e backtrace

let context_async_ignore f =
  Monitor.handle_errors f (capture_exception)

let context_async_or_error f =
  context_async (fun () ->
    f ()
    >>| capture_and_return_or_error)