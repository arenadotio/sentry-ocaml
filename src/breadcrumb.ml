open Core_kernel

type level =
  [ `Critical
  | `Error
  | `Warning
  | `Info
  | `Debug ]
[@@deriving sexp_of]

let level_to_string = function
  | `Critical -> "critical"
  | `Error -> "error"
  | `Warning -> "warning"
  | `Info -> "info"
  | `Debug -> "debug"

type t =
  { timestamp : Time.t sexp_opaque
  ; type_ : string
  ; message : string option
  ; data : Json.t String.Map.t
  ; category : string option
  ; level : level }
[@@deriving sexp_of]

let make ?timestamp ?(type_="default") ?message ?(data=String.Map.empty)
      ?category ?(level=`Info) () =
  let timestamp = match timestamp with
    | Some timestamp -> timestamp
    | None -> Time.now ()
  in
  { timestamp ; type_ ; message ; data ; category ; level }

let make_navigation ?timestamp ?message ?category ?level ~from ~to_ () =
  let data =
    [ "from", `String from
    ; "to", `String to_ ]
    |> String.Map.of_alist_exn
  in
  make ?timestamp ?message ?category ?level ~data ~type_:"navigation" ()

let make_http ?timestamp ?message ?category ?level ~url ~method_ ~status_code
      ~reason () =
  let data =
    [ "url", `String url
    ; "method", `String method_
    ; "status_code", `Int status_code
    ; "reason", `String reason ]
    |> String.Map.of_alist_exn
  in
  make ?timestamp ?message ?category ?level ~data ~type_:"http" ()

let to_payload t =
  { Payloads_t.timestamp = t.timestamp
  ; type_ = Some t.type_
  ; message = t.message
  ; data = Some (Map.to_alist t.data)
  ; category = t.category
  ; level = Some (level_to_string t.level) }
