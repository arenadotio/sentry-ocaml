open Core_kernel

type level =
  [ `Critical
  | `Error
  | `Warning
  | `Info
  | `Debug ]
[@@deriving sexp_of]

type t = private
  { timestamp : Time.t
  ; type_ : string
  ; message : string option
  ; data : Json.t String.Map.t
  ; category : string option
  ; level : level }
[@@deriving sexp_of]

val make
  : ?timestamp:Time.t
  -> ?type_:string
  -> ?message:string
  -> ?data:Json.t String.Map.t
  -> ?category:string
  -> ?level:level
  -> unit
  -> t

  val make_navigation
  : ?timestamp:Time.t
  -> ?message:string
  -> ?category:string
  -> ?level:level
  -> from:string
  -> to_:string
  -> unit
  -> t

  val make_http
  : ?timestamp:Time.t
  -> ?message:string
  -> ?category:string
  -> ?level:level
  -> url:string
  -> method_:string
  -> status_code:int
  -> reason:string
  -> unit
  -> t

val to_payload : t -> Payloads_t.breadcrumb
