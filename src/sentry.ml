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

let make_key name sexp_of =
  let name = "sentry_" ^ name in
  Univ_map.Key.create ~name sexp_of

let dsn_key = make_key "dsn" [%sexp_of: Dsn.t]
let environment_key = make_key "environment" [%sexp_of: string]
let release_key = make_key "release" [%sexp_of: string]
let server_name_key = make_key "server_name" [%sexp_of: string]

let env_environment = Sys.getenv "SENTRY_ENVIRONMENT"
let env_release = Sys.getenv "SENTRY_RELEASE"
let env_server_name = Sys.getenv "SENTRY_NAME"

let with_config_item key value f =
  Scheduler.with_local key (Some value) ~f

let with_dsn value f = with_config_item dsn_key value f
let with_environment value f = with_config_item environment_key value f
let with_release value f = with_config_item release_key value f
let with_server_name value f = with_config_item server_name_key value f

(* Is there a better way to write this function? *)
let rec with_config ?dsn ?environment ?release ?server_name f =
  match dsn with
  | Some dsn ->
    with_dsn dsn (fun () ->
      with_config ?environment ?release ?server_name f)
  | None ->
    match environment with
    | Some environment ->
      with_environment environment (fun () ->
        with_config ?release ?server_name f)
    | None ->
      match release with
      | Some release ->
        with_release release (fun () ->
          with_config ?server_name f)
      | None ->
        match server_name with
        | Some server_name ->
          with_server_name server_name f
        | None ->
          f ()

let find_config key default =
  Option.first_some (Scheduler.find_local key) default

let make_event ?exn ?message () =
  let environment = find_config environment_key env_environment in
  let release = find_config release_key env_release  in
  let server_name = find_config server_name_key env_server_name in
  let exn = Option.map exn ~f:List.return in
  Event.make ?exn ?message ?environment ?release ?server_name ()

let capture_event ?exn ?message () =
  let dsn =
    Scheduler.find_local dsn_key
    |> Option.value ~default:Dsn.default
  in
  match dsn with
  | Some dsn ->
    let event = make_event ?exn ?message () in
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
  let message = Message.make ~message:(Error.to_string_hum err) () in
  capture_event ~message ~exn ()

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