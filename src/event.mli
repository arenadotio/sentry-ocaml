(** https://docs.sentry.io/clientdev/attributes/ *)
open Core

type t =
  { event_id : Uuidm.t
  ; timestamp : Time.t
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
  ; exception_ : Exception.t list option
  ; message : Message.t option }

val make
  : ?event_id:Uuidm.t
  -> ?timestamp:Time.t
  -> ?logger:string
  -> ?platform:Platform.t
  -> ?sdk:Sdk.t
  -> ?level:Severity_level.t
  -> ?culprit:string
  -> ?server_name:string
  -> ?release:string
  -> ?tags:string String.Map.t
  -> ?environment:string
  -> ?modules:string String.Map.t
  -> ?extra:string String.Map.t
  -> ?fingerprint:string list
  -> ?message:Message.t
  -> ?exn:Exception.t list
  -> unit
  -> t

val to_payload : t -> Payloads_t.event

(** Converts [t] to the Sentry JSON representation, suitable to be uploaded. *)
val to_json_string : t -> string