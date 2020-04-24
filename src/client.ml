open Core_kernel
open Async_kernel
open Async_unix

let user_agent = sprintf "%s/%s" Config.name Config.version

let dsn_to_auth_header { Dsn.uri; public_key; private_key; _ } time =
  let value =
    let base =
      sprintf
        "Sentry sentry_version=7, sentry_client=%s, sentry_timestamp=%d, sentry_key=%s"
        user_agent
        Time.(to_span_since_epoch time |> Span.to_sec |> Float.iround_exn)
        public_key
    in
    (* Only had a private key if we have one and we're talking over a secure
       channel *)
    match Uri.scheme uri, private_key with
    | Some "https", Some private_key -> sprintf "%s, sentry_secret=%s" base private_key
    | _ -> base
  in
  Cohttp.Header.init_with "X-Sentry-Auth" value
;;

let make_headers ~dsn timestamp =
  dsn_to_auth_header dsn timestamp
  |> Fn.flip Cohttp.Header.prepend_user_agent user_agent
  |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
;;

let rec send_request ~headers ~data uri =
  let body = Cohttp_async.Body.of_string data in
  let%bind response, body = Cohttp_async.Client.post ~headers ~body uri in
  if Cohttp.Response.status response
     |> Cohttp.Code.code_of_status
     |> Cohttp.Code.is_redirection
  then
    Cohttp.Response.headers response
    |> Cohttp.Header.get_location
    |> function
    | None -> failwithf "Redirect with no Location header from %s" (Uri.to_string uri) ()
    | Some uri -> send_request ~headers ~data uri
  else return (response, body)
;;

let send_event_and_wait_exn ~dsn event =
  let headers = make_headers ~dsn event.Event.timestamp in
  let uri = Dsn.event_store_uri dsn in
  let data = Event.to_json_string event in
  let%bind response, body = send_request ~headers ~data uri in
  match Cohttp.Response.status response with
  | `OK ->
    Cohttp_async.Body.to_string body
    >>| Payloads_j.response_of_string
    >>| fun { Payloads_j.id } -> id
  | status ->
    let errors =
      Cohttp.Response.headers response
      |> Fn.flip Cohttp.Header.get "X-Sentry-Error"
      |> Option.value ~default:"No X-Sentry-Error header"
    in
    failwithf
      "Unexpected %d response from Sentry: %s"
      (Cohttp.Code.code_of_status status)
      errors
      ()
;;

let send_event_and_wait ~dsn event =
  Monitor.try_with (fun () -> send_event_and_wait_exn ~dsn event)
  >>| function
  | Ok id ->
    Log.Global.info "Successfully uploaded sentry event %s" (Uuid.unwrap event.event_id);
    Some id
  | Error e ->
    Exn.to_string e |> Log.Global.error "Failed to upload Sentry event: %s";
    None
;;

let event_pipe =
  let reader, writer = Pipe.create () in
  (* Use a pipe to let us sent events asynchronously and still ensure that they're
     all written before the program exits *)
  let close p =
    if not (Pipe.is_closed p)
    then (
      Pipe.close p;
      Pipe.upstream_flushed p >>| ignore)
    else return ()
  in
  let consumer = Pipe.add_consumer reader ~downstream_flushed:(Fn.const (return `Ok)) in
  Pipe.iter ~consumer reader ~f:(fun (dsn, event) ->
      send_event_and_wait ~dsn event
      >>| fun _ -> Pipe.Consumer.values_sent_downstream consumer)
  |> don't_wait_for;
  Shutdown.at_shutdown (fun () ->
      close writer
      >>= fun () -> Pipe.downstream_flushed writer >>= fun _ -> Log.Global.flushed ());
  Gc.add_finalizer_exn writer (Fn.compose don't_wait_for close);
  writer
;;

let send_event ~dsn event = Pipe.write_without_pushback_if_open event_pipe (dsn, event)
