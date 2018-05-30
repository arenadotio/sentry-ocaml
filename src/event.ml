open Core

type sdk_info = Payloads_t.sdk_info =
  { name : string
  ; version: string
  ; integrations : string list option }

type message = Payloads_t.message =
  { message : string
  ; params : string list option
  ; formatted : string }

type t = Payloads_t.event =
  { event_id : Uuidm.t
  ; timestamp : Time.t
  ; logger : string
  ; platform : Platform.t
  ; sdk : sdk_info
  ; level : Severity_level.t option
  ; culprit : string option
  ; server_name : string option
  ; release : string option
  ; tags : (string * string) list option
  ; environment : string option
  ; modules : (string * string) list option
  ; extra : (string * string) list option
  ; fingerprint : string list option
  ; message : message option }

let base_sdk =
  { name = Config.name
  ; version = Config.version
  ; integrations = None }

let make ?event_id ~timestamp ?(logger="ocaml") ?(platform=`Other)
      ?(sdk=base_sdk) ?level ?culprit ?server_name ?release ?tags ?environment
      ?modules ?extra ?fingerprint ?message () =
  let event_id =
    match event_id with
    | Some id -> id
    | None -> Uuidm.create `V4
  in
  let message =
    match message with
    | Some message ->
      Some { message
           ; params = None
           ; formatted = message }
    | None -> None
  in
  { event_id ; timestamp ; logger ; platform ; sdk ; level ; culprit
  ; server_name ; release ; tags ; environment ; modules ; extra ; fingerprint
  ; message }

let to_json_string t =
  Payloads_j.string_of_event t
