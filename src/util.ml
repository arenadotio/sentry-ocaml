open Core_kernel
open Async

let cap_string_length ?(max_len = 512) str =
  if String.length str > max_len then String.sub ~pos:0 ~len:max_len str else str
;;

let empty_list_option l =
  match l with
  | [] -> None
  | l -> Some l
;;

let map_to_alist_option m = Map.to_alist m |> empty_list_option

let with_print_exn f =
  try f () with
  | e -> Exn.to_string e |> print_endline
;;
