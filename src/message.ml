open Core_kernel
open Util

type t =
  { message : string
  ; params : string list
  ; formatted : string }

let make ~message ?(params=[]) ?formatted () =
  let formatted = Option.value formatted ~default:message in
  { message ; params ; formatted }

let to_payload { message ; params ; formatted } =
  { Payloads_t.message ; params = empty_list_option params ; formatted }