open Core_kernel
open Async_kernel
open Async_unix

module Config = Config
module Dsn = Dsn
module Event = Event
module Exception = Exception
module Platform = Platform
module Sdk = Sdk
module Severity_level = Severity_level

let user_agent =
  sprintf "%s/%s" Config.name Config.version

let dsn_to_auth_header { Dsn.uri ; public_key ; private_key } time =
  let value =
    let base =
      sprintf "Sentry sentry_version=7, \
               sentry_client=%s, \
               sentry_timestamp=%d, \
               sentry_key=%s"
        user_agent
        Time.(to_span_since_epoch time |> Span.to_sec |> Float.iround_exn)
        public_key
    in
    (* Only had a private key if we have one and we're talking over a secure
       channel *)
    match (Uri.scheme uri), private_key with
    | Some "https", Some private_key ->
      sprintf "%s, sentry_secret=%s" base private_key
    | _ -> base
  in
  Cohttp.Header.init_with "X-Sentry-Auth" value

let make_headers ~dsn timestamp =
  dsn_to_auth_header dsn timestamp
  |> Fn.flip Cohttp.Header.prepend_user_agent user_agent
  |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"

let rec send_request ~headers ~data uri =
  let body = Cohttp_async.Body.of_string data in
  let%bind response, body = Cohttp_async.Client.post ~headers ~body uri in
  if Cohttp.Response.status response
     |> Cohttp.Code.code_of_status
     |> Cohttp.Code.is_redirection then
    Cohttp.Response.headers response
    |> Cohttp.Header.get_location
    |> function
    | None ->
      failwithf "Redirect with no Location header from %s"
        (Uri.to_string uri) ()
    | Some uri ->
      send_request ~headers ~data uri
  else
    return (response, body)

let capture_event' ?(dsn=Dsn.default) event =
  match dsn with
  | None -> return None
  | Some dsn ->
    let headers = make_headers dsn event.Event.timestamp in
    let uri = Dsn.event_store_uri dsn in
    let data = Event.to_json_string event in
    let%bind response, body = send_request ~headers ~data uri in
    match Cohttp.Response.status response with
    | `OK ->
      Cohttp_async.Body.to_string body
      >>| Payloads_j.response_of_string
      >>| fun { Payloads_j.id } -> Some id
    | status ->
      let errors =
        Cohttp.Response.headers response
        |> Fn.flip Cohttp.Header.get "X-Sentry-Error"
        |> Option.value ~default:"No X-Sentry-Error header"
      in
      failwithf "Unexpected %d response from Sentry: %s"
        (Cohttp.Code.code_of_status status) errors ()

let event_pipe =
  let reader, writer = Pipe.create () in
  (* Use a pipe to let us sent events asynchronously and still ensure that they're
     all written before the program exits *)
  let close p =
    if not (Pipe.is_closed p) then begin
      Pipe.close p;
      Pipe.upstream_flushed p
      >>| ignore
    end else
      return ()
  in
  let consumer = Pipe.add_consumer reader
                   ~downstream_flushed:(Fn.const (return `Ok)) in
  Pipe.iter ~consumer reader ~f:(fun (dsn, event) ->
    capture_event' ?dsn event
    >>| fun _ -> Pipe.Consumer.values_sent_downstream consumer)
  |> don't_wait_for;
  Shutdown.at_shutdown (fun () ->
    close writer
    >>= fun () ->
    Pipe.downstream_flushed writer
    |> Deferred.ignore);
  Gc.add_finalizer_exn writer (Fn.compose don't_wait_for close);
  writer

let capture_event ?dsn event =
  Pipe.write_without_pushback_if_open event_pipe (dsn, event)

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