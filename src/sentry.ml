open Core_kernel
open Async_kernel
open Async_unix

module Client = Client
module Config = Config
module Context = Context
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
let context_key = make_key "context" [%sexp_of: Context.t]

let with_thread_local key value f =
  Scheduler.with_local key (Some value) ~f

let with_dsn value f = with_thread_local dsn_key value f

let with_context value f = with_thread_local context_key value f

let default_context = lazy (Context.default ())

let find_context () =
  Scheduler.find_local context_key
  |> function
  | Some context -> context
  | None -> Lazy.force default_context

let with_new_context f =
  let context = find_context () |> Context.copy in
  with_context context (fun () -> f context)

let set_environment env =
  (find_context ()).environment <- Some env

let set_server_name name =
  (find_context ()).server_name <- Some name

let set_release release =
  (find_context ()).release <- Some release

let merge_tags tags =
  find_context ()
  |> Context.merge_tags tags

let make_event ?tags ?exn ?message () =
  let context = find_context () in
  let exn = Option.map exn ~f:List.return in
  Event.make ?exn ?message ~context ?tags ()

let capture_event ?tags ?exn ?message () =
  let dsn =
    Scheduler.find_local dsn_key
    |> Option.value ~default:Dsn.default
  in
  match dsn with
  | Some dsn ->
    let event = make_event ?tags ?exn ?message () in
    Log.Global.info "Uploading sentry event %s" (Uuid.unwrap event.event_id);
    Client.send_event ~dsn event
  | _ ->
    Log.Global.info "Not uploading Sentry event because no DSN is set"

let capture_message ?tags message =
  let message = Message.make ~message () in
  capture_event ?tags ~message ()

let capture_exception ?tags ?message exn =
  let exn = Exception.of_exn exn in
  let message =
    Option.map message ~f:(fun message ->
      Message.make ~message ())
  in
  capture_event ?tags ?message ~exn ()

let capture_error ?tags err =
  let exn = Exception.of_error err in
  let message = Message.make ~message:(Error.to_string_hum err) () in
  capture_event ?tags ~message ~exn ()

let with_exn_handler f =
  try
    f ()
  with e ->
    let backtrace = Caml.Printexc.get_raw_backtrace () in
    capture_exception e;
    Caml.Printexc.raise_with_backtrace e backtrace

let with_exn_handler_ignore f =
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

let with_error_and_exn_handler f =
  with_exn_handler (fun () ->
    f ()
    |> capture_and_return_or_error)

let with_async_exn_handler f =
  Monitor.try_with ~extract_exn:false ~rest:(`Call capture_exception) f
  >>= function
  | Ok res -> return res
  | Error e ->
    let backtrace = Caml.Printexc.get_raw_backtrace () in
    capture_exception e;
    Caml.Printexc.raise_with_backtrace e backtrace

let with_async_exn_handler_ignore f =
  Monitor.handle_errors f capture_exception

let with_async_error_and_exn_handler f =
  with_async_exn_handler (fun () ->
    f ()
    >>| capture_and_return_or_error)
