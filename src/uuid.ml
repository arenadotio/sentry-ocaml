open Core_kernel

type t = Uuidm.t

let unwrap t = Uuidm.to_bytes t |> Hex.of_string |> Hex.show

let wrap s =
  Hex.to_string (`Hex s)
  |> Uuidm.of_bytes
  |> function
  | Some uuid -> uuid
  | None -> failwithf "Failed to parse %s as a UUID" s ()
;;

let%test_unit "wrap" =
  let expect = "fc6d8c0c43fc4630ad850ee518f1b9d0" in
  expect |> wrap |> unwrap |> [%test_result: string] ~expect
;;
