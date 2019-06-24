open Base
open Sexplib.Conv

type t = string [@@deriving sexp_of]

let unwrap t =
  Util.cap_string_length ~max_len:512 t

let wrap t = t

let%test_unit "round-trip" =
  let expect = "adsfasdfsdsd131" in
  expect
  |> wrap
  |> unwrap
  |> [%test_result: string] ~expect

let%test_unit "long string round trip" =
  let input = String.init 1000 ~f:(Fn.const 'a') in
  let expect = String.sub ~pos:0 ~len:512 input in
  input
  |> wrap
  |> unwrap
  |> [%test_result: string] ~expect
