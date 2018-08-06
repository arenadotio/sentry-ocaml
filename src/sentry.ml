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
let tags_key = make_key "tags" [%sexp_of: string String.Map.t]

let default_environment = Sys.getenv "SENTRY_ENVIRONMENT"
let default_release = Sys.getenv "SENTRY_RELEASE"

let default_server_name =
  match Sys.getenv "SENTRY_NAME" with
  | None -> Some (Unix.gethostname ())
  | value -> value

let default_tags =
  (* TODO: Add system username (Unix.getlogin) and cwd (Sys.getcwd). We need to
     handle Deferred in here for that to work *)
  [ "os_type", Sys.os_type
  ; "executable_name", Sys.executable_name
  (* TODO: Include this as JSON once #5 is done *)
  ; "argv", Sys.argv |> Array.to_list |> String.concat ~sep:" "
  ; "backend_type", (match Caml.Sys.backend_type with
      | Caml.Sys.Native -> "Native"
      | Bytecode -> "Bytecode"
      | Other o -> o) ]
  |> String.Map.of_alist_exn
  |> Option.some

let with_config_item key value f =
  Scheduler.with_local key (Some value) ~f

let with_dsn value f = with_config_item dsn_key value f
let with_environment value f = with_config_item environment_key value f
let with_release value f = with_config_item release_key value f
let with_server_name value f = with_config_item server_name_key value f

let with_tags tags f =
  let tags = String.Map.of_alist_reduce tags ~f:(fun a b ->
    if String.(a <> b) then
      Log.Global.error "Duplicate tags: %s and %s. Ignoring %s." a b b;
    a) in
  (* Merge with existing tags *)
  let value =
    Scheduler.find_local tags_key
    |> Option.value_map ~default:tags ~f:(fun existing_tags ->
      Map.merge_skewed tags existing_tags ~combine:(fun ~key:_ new_tag _ ->
        new_tag))
  in
  with_config_item tags_key value f

(* Is there a better way to write this function? *)
let rec with_config ?dsn ?environment ?release ?server_name ?tags f =
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
          match tags with
          | Some tags ->
            with_tags tags f
          | None ->
            f ()

let find_config key default =
  Option.first_some (Scheduler.find_local key) default

let find_environment () = find_config environment_key default_environment
let find_release () = find_config release_key default_release
let find_server_name () = find_config server_name_key default_server_name
let find_tags () =
  match Scheduler.find_local tags_key, default_tags with
  | None, default -> default
  | (Some _) as tags, None -> tags
  | Some tags, Some default ->
    Map.merge_skewed tags default ~combine:(fun ~key:_ new_tag _ ->
      new_tag)
    |> Option.some

let%test_unit "tag merging" =
  let find_tag () =
    find_tags ()
    |> Option.value_exn
    |> Fn.flip Map.find "test"
  in
  with_tags [ "test", "first" ] @@ fun () ->
  find_tag ()
  |> [%test_result: string option] ~expect:(Some "first");
  with_tags [ "test", "second" ] begin fun () ->
    find_tag ()
    |> [%test_result: string option] ~expect:(Some "second")
  end;
  find_tag ()
  |> [%test_result: string option] ~expect:(Some "first")

let make_event ?(tags=[]) ?exn ?message () =
  with_tags tags @@ fun () ->
  let environment = find_environment () in
  let release = find_release () in
  let server_name = find_server_name () in
  let tags = find_tags () in
  let exn = Option.map exn ~f:List.return in
  Event.make ?exn ?message ?environment ?release ?server_name ?tags ()

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

let context ?tags f =
  try
    f ()
  with e ->
    let backtrace = Caml.Printexc.get_raw_backtrace () in
    capture_exception ?tags e;
    Caml.Printexc.raise_with_backtrace e backtrace

let context_ignore ?tags f =
  try
    f ()
  with e ->
    capture_exception ?tags e

let capture_and_return_or_error ?tags v =
  match v with
  | Ok _ -> v
  | Error e ->
    capture_error ?tags e;
    v

let context_or_error ?tags f =
  context (fun () ->
    f ()
    |> capture_and_return_or_error ?tags)

let context_async ?tags f =
  Monitor.try_with ~extract_exn:false ~rest:(`Call (capture_exception ?tags)) f
  >>= function
  | Ok res -> return res
  | Error e ->
    let backtrace = Caml.Printexc.get_raw_backtrace () in
    capture_exception ?tags e;
    Caml.Printexc.raise_with_backtrace e backtrace

let context_async_ignore ?tags f =
  Monitor.handle_errors f (capture_exception ?tags)

let context_async_or_error ?tags f =
  context_async (fun () ->
    f ()
    >>| capture_and_return_or_error ?tags)
