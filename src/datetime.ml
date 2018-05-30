open Core

type t = Time.t

let unwrap t =
  Time.to_string_iso8601_basic ~zone:Time.Zone.utc t
  |> String.rstrip ~drop:((=) 'Z')

let wrap s =
  Time.of_string s

let%test_unit "round-trip" =
  let expect = "2011-05-02T17:41:36" in
  expect
  |> wrap
  |> unwrap
  |> [%test_result: string] ~expect
