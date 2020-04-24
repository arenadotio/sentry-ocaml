(** https://docs.sentry.io/clientdev/attributes/#required-attributes *)
open Core_kernel

type t = private
  { name : string
  ; version : string
  ; integrations : String.Set.t
  }
[@@deriving sexp_of]

val make : name:string -> version:string -> ?integrations:String.Set.t -> unit -> t
val default : t
val to_payload : t -> Payloads_t.sdk_info
