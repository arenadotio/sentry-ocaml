open Core_kernel
open Util

type t =
  { event_id : Uuidm.t sexp_opaque
  ; timestamp : Time.t sexp_opaque
  ; logger : string
  ; platform : Platform.t
  ; sdk : Sdk.t
  ; level : Severity_level.t option
  ; culprit : string option
  ; server_name : string option
  ; release : string option
  ; tags : string String.Map.t
  ; environment : string option
  ; modules : string String.Map.t
  ; extra : string String.Map.t
  ; fingerprint : string list option
  ; exception_ : Exception.t list option sexp_opaque
  ; message : Message.t option }
[@@deriving sexp_of]

let make ?event_id ?timestamp ?(logger="ocaml") ?(platform=`Other)
      ?(sdk=Sdk.default) ?level ?culprit ?server_name ?release
      ?(tags=String.Map.empty) ?environment
      ?(modules=String.Map.empty) ?(extra=String.Map.empty) ?fingerprint
      ?message ?exn () =
  let event_id =
    match event_id with
    | Some id -> id
    | None -> Uuidm.create `V4
  in
  let timestamp =
    match timestamp with
    | Some ts -> ts
    | None -> Time.now ()
  in
  { event_id ; timestamp ; logger ; platform ; sdk ; level ; culprit
  ; server_name ; release ; tags ; environment ; modules ; extra ; fingerprint
  ; message ; exception_ = exn }

let to_payload { event_id ; timestamp ; logger ; platform ; sdk ; level
               ; culprit ; server_name ; release ; tags ; environment ; modules
               ; extra ; fingerprint ; exception_ ; message } =
  { Payloads_t.event_id ; timestamp ; logger ; platform
  ; sdk = Sdk.to_payload sdk
  ; level ; culprit ; server_name ; release
  ; tags = map_to_alist_option tags
  ; environment
  ; modules = map_to_alist_option modules
  ; extra = map_to_alist_option extra
  ; fingerprint
  ; exception_ = Option.map ~f:Exception.list_to_payload exception_
  ; message = Option.map ~f:Message.to_payload message }

let to_json_string t =
  to_payload t
  |> Payloads_j.string_of_event
