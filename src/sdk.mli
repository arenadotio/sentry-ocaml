(** https://docs.sentry.io/clientdev/attributes/#required-attributes *)
open Core

type t = private
  { name : string
  ; version: string
  ; integrations : String.Set.t }

val make
  : name:string
  -> version:string
  -> ?integrations:String.Set.t
  -> unit
  -> t

val default : t

val to_payload : t -> Payloads_t.sdk_info