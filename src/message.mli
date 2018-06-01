(** https://docs.sentry.io/clientdev/interfaces/message/ *)
type t = private
  { message : string
  ; params : string list
  ; formatted : string }

val make
  : message:string
  -> ?params:string list
  -> ?formatted:string
  -> unit
  -> t

val to_payload : t -> Payloads_t.message