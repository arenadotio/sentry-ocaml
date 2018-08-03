(** https://docs.sentry.io/clientdev/attributes/#required-attributes *)
type t =
  [ `As3
  | `C
  | `Cfml
  | `Cocoa
  | `Csharp
  | `Go
  | `Java
  | `Javascript
  | `Node
  | `Objc
  | `Other
  | `Perl
  | `Php
  | `Python
  | `Ruby ]
[@@deriving sexp_of]

val wrap : string -> [> t]
val unwrap : [< t] -> string
