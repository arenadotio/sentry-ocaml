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

let capture_event ?(dsn=Dsn.default) event =
  match dsn with
  | Some dsn ->
    Client.send_event ~dsn event
  | _ -> ()

let capture_message ?dsn message =
  let message = Message.make ~message () in
  Event.make ~message ()
  |> capture_event ?dsn

let capture_exception ?dsn ?message exn =
  let exn = Exception.of_exn exn in
  let message =
    Option.map message ~f:(fun message ->
      Message.make ~message ())
  in
  Event.make ?message ~exn:[ exn ] ()
  |> capture_event ?dsn

let capture_error ?dsn err =
  let exn = Exception.of_error err in
  Event.make ~exn:[ exn ] ()
  |> capture_event ?dsn

let context ?dsn f =
  try
    f ()
  with e ->
    let backtrace = Caml.Printexc.get_raw_backtrace () in
    capture_exception ?dsn e;
    Caml.Printexc.raise_with_backtrace e backtrace

let context_ignore ?dsn f =
  try
    f ()
  with e ->
    capture_exception ?dsn e

let capture_and_return_or_error ?dsn v =
  match v with
  | Ok _ -> v
  | Error e ->
    capture_error ?dsn e;
    v

let context_or_error ?dsn f =
  context ?dsn (fun () ->
    f ()
    |> capture_and_return_or_error ?dsn)

let context_async ?dsn f =
  Monitor.try_with ~extract_exn:false ~rest:(`Call (capture_exception ?dsn)) f
  >>= function
  | Ok res -> return res
  | Error e ->
    let backtrace = Caml.Printexc.get_raw_backtrace () in
    capture_exception ?dsn e;
    Caml.Printexc.raise_with_backtrace e backtrace

let context_async_ignore ?dsn f =
  Monitor.handle_errors f (capture_exception ?dsn)

let context_async_or_error ?dsn f =
  context_async ?dsn (fun () ->
    f ()
    >>| capture_and_return_or_error ?dsn)