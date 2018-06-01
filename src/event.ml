open Core

type sdk_info = Payloads_t.sdk_info =
  { name : string
  ; version: string
  ; integrations : string list option }

type mechanism = Payloads_t.mechanism =
  { type_ : string
  ; description : string option
  ; help_link : string option
  ; handled : bool option
  (* TODO: meta *)
  ; data : (string * string) list option }

type exception_value = Payloads_t.exception_value =
  { type_ : string
  ; value : string
  ; module_ : string option
  ; thread_id : string option
  ; mechanism : mechanism option }

type exception_ = Payloads_t.exception_ =
  { values : exception_value list }

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
  ; exception_ : exception_ option
  ; message : message option }

let base_sdk =
  { name = Config.name
  ; version = Config.version
  ; integrations = None }

let make ?event_id ~timestamp ?(logger="ocaml") ?(platform=`Other)
      ?(sdk=base_sdk) ?level ?culprit ?server_name ?release ?tags ?environment
      ?modules ?extra ?fingerprint ?message ?exn () =
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
  let exception_ =
    match exn with
    | Some exn ->
      Some { values = [{ type_ = Caml.Printexc.exn_slot_name exn
                       ; value = Caml.Printexc.to_string exn 
                       ; module_ = None
                       ; thread_id = None
                       ; mechanism = None }] }
    | None -> None
  in
  { event_id ; timestamp ; logger ; platform ; sdk ; level ; culprit
  ; server_name ; release ; tags ; environment ; modules ; extra ; fingerprint
  ; message ; exception_ }

let to_json_string t =
  Payloads_j.string_of_event t
