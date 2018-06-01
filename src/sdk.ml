open Core
open Util

type t =
  { name : string
  ; version: string
  ; integrations : String.Set.t }

let make ~name ~version ?(integrations=String.Set.empty) () =
  { name ; version ; integrations }

let default =
  make ~name:Config.name ~version:Config.version ()

let to_payload { name ; version ; integrations } =
  { Payloads_t.name ; version
  ; integrations =
      String.Set.to_list integrations
      |> empty_list_option }