open Core_kernel
open Util

type t =
  { name : string
  ; version: string
  ; integrations : String.Set.t }
[@@deriving sexp_of]

let make ~name ~version ?(integrations=String.Set.empty) () =
  { name ; version ; integrations }

let default =
  make ~name:Config.name ~version:Config.version ()

let to_payload { name ; version ; integrations } =
  { Payloads_t.name ; version
  ; integrations =
      String.Set.to_list integrations
      |> empty_list_option }