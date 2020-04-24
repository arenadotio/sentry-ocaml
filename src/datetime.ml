open Core_kernel

type t = Time.t

let unwrap t =
  Time.to_string_iso8601_basic ~zone:Time.Zone.utc t
  |> String.rstrip ~drop:(Char.( = ) 'Z')
;;

let wrap s =
  Time.of_string_gen
    ~default_zone:(fun () -> Time.Zone.utc)
    ~find_zone:(fun s -> failwithf "Unexpected time zone: %s" s ())
    s
;;

let%test_unit "round-trip" =
  let expect = "2011-05-02T17:41:36.000000" in
  expect |> wrap |> unwrap |> [%test_result: string] ~expect
;;
