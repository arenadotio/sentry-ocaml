(** https://docs.sentry.io/clientdev/attributes/#required-attributes *)
type t =
  [ `Fatal
  | `Error
  | `Warning
  | `Info
  | `Debug
  ]
[@@deriving sexp_of]

val wrap : string -> [> t ]
val unwrap : [< t ] -> string
