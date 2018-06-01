open Core

let empty_list_option l =
  match l with
  | [] -> None
  | l -> Some l

let map_to_alist_option m =
  Map.to_alist m
  |> empty_list_option