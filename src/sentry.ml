open Core_kernel
open Async_kernel

module Config = Config
module Event = Event
module Exception = Exception
module Platform = Platform
module Sdk = Sdk
module Severity_level = Severity_level

type t' =
  { uri : Uri.t
  ; public_key : string
  ; private_key : string option
  ; project_id : int }
[@@deriving sexp_of]

type t = t' option [@@deriving sexp_of]

(* Informative data we send Sentry *)

let empty =
  None

let of_dsn dsn =
  if Uri.(dsn = empty) then
    Ok empty
  else
    let required =
      [ "SCHEME", Uri.scheme dsn
      ; "HOST", Uri.host dsn
      ; "PUBLIC_KEY", Uri.user dsn
      ; "PROJECT_ID",
        Uri.path dsn
        |> String.rsplit2 ~on:'/'
        |> Option.map ~f:snd ]
    in
    List.filter_map required ~f:(fun (name, value) ->
      match value with
      | None -> Some name
      | Some _ -> None)
    |> function
    | [] ->
      begin match List.map required ~f:snd with
      | [ Some scheme ; Some host ; Some public_key ; Some project_id ] ->
        let private_key = Uri.password dsn in
        let uri = Uri.make ~scheme ~host () in
        begin try
          let project_id = Int.of_string project_id in
          Ok (Some { uri ; public_key ; private_key ; project_id })
        with _ ->
          Or_error.errorf "Invalid PROJECT_ID: %s (should be an integer)"
            project_id
        end
      | _ -> assert false
      end
    | missing ->
      String.concat missing ~sep:","
      |> Or_error.errorf "Missing required DSN field(s): %s"

let of_dsn_exn dsn =
  of_dsn dsn
  |> Or_error.ok_exn

let user_agent =
  sprintf "%s/%s" Config.name Config.version

let to_auth_header { uri ; public_key ; private_key } time =
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

let make_headers t timestamp =
  to_auth_header t timestamp
  |> Fn.flip Cohttp.Header.prepend_user_agent user_agent
  |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"

let make_uri { uri ; project_id } =
  sprintf "/api/%d/store" project_id
  |> Uri.with_path uri

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

let capture_event t event =
  match t with
  | None -> return None
  | Some t ->
    let headers = make_headers t event.Event.timestamp in
    let uri = make_uri t in
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

let capture_message t message =
  let message = Message.make ~message () in
  Event.make ~message ()
  |> capture_event t

let capture_exception t ?message exn =
  let exn = Exception.of_exn exn in
  let message =
    Option.map message ~f:(fun message ->
      Message.make ~message ())
  in
  Event.make ?message ~exn:[ exn ] ()
  |> capture_event t

let context t f =
  Monitor.try_with ~extract_exn:true
    ~rest:(`Call (fun e -> capture_exception t e |> ignore)) f
  >>= function
  | Ok res -> return res
  | Error e ->
    capture_exception t e
    >>| fun _ ->
    raise e