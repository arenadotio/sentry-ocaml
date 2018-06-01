open Core_kernel

type t =
  [ `Fatal
  | `Error
  | `Warning
  | `Info
  | `Debug ]
[@@deriving sexp_of]

let unwrap = function
  | `Fatal -> "fatal"
  | `Error -> "error"
  | `Warning -> "warning"
  | `Info -> "info"
  | `Debug -> "debug"

let wrap = function
  | "fatal" -> `Fatal
  | "error" -> `Error
  | "warning" -> `Warning
  | "info" -> `Info
  | "debug" -> `Debug
  | s -> failwithf "Unknown severity level %s" s ()
