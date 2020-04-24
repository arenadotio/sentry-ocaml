open Core_kernel
open Util

type t =
  { message : string
  ; params : string list
  ; formatted : string option
  }
[@@deriving sexp_of]

let make ~message ?(params = []) ?formatted () = { message; params; formatted }

let to_payload { message; params; formatted } =
  { Payloads_t.message; params = empty_list_option params; formatted }
;;

let%expect_test "to_payload with params" =
  make
    ~message:"My raw message with interpolated strings like %s"
    ~params:[ "this"; "is an example" ]
    ()
  |> to_payload
  |> Payloads_j.string_of_message
  |> print_endline;
  [%expect
    {| {"message":"My raw message with interpolated strings like %s","params":["this","is an example"]} |}]
;;

let%expect_test "to_payload without params" =
  make ~message:"Lorem ipsum" ()
  |> to_payload
  |> Payloads_j.string_of_message
  |> print_endline;
  [%expect {| {"message":"Lorem ipsum"} |}]
;;
