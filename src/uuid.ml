open Core

type t = Uuidm.t

let unwrap t =
  Uuidm.to_string t

let wrap s =
  Uuidm.of_string s
  |> function
  | Some uuid -> uuid
  | None -> failwithf "Failed to parse %s as a UUID" s ()

let%test_unit "wrap" =
  let expect = "fc6d8c0c43fc4630ad850ee518f1b9d0" in
  expect
  |> wrap
  |> unwrap
  |> [%test_result: string] ~expect
