open Core

type t' =
  { uri : Uri_sexp.t
  ; public_key : string
  ; private_key : string option
  ; project_id : int }
[@@deriving sexp_of]

type t = t' option [@@deriving sexp_of]

let make ~uri ~public_key ?private_key ~project_id () =
  Some { uri ; public_key ; private_key ; project_id }

let of_uri_exn dsn =
  let required =
    [ "SCHEME", Uri.scheme dsn
    ; "HOST", Uri.host dsn
    ; "PUBLIC_KEY", Uri.user dsn
    ; "PROJECT_ID",
      Uri.path dsn
      |> String.rsplit2 ~on:'/'
      |> Option.map ~f:snd ]
  in
  List.filter_map required ~f:(fun (name, value) ->
    match value with
    | None -> Some name
    | Some _ -> None)
  |> function
  | [] ->
    begin match List.map required ~f:snd with
    | [ Some scheme ; Some host ; Some public_key ; Some project_id ] ->
      let private_key = Uri.password dsn in
      let uri = Uri.make ~scheme ~host () in
      begin try
        let project_id = Int.of_string project_id in
        make ~uri ~public_key ?private_key ~project_id ()
      with _ ->
        failwithf "Invalid PROJECT_ID: %s (should be an integer)"
          project_id ()
      end
    | _ -> assert false
    end
  | missing ->
    let fields = String.concat missing ~sep:"," in
    failwithf "Missing required DSN field(s): %s" fields ()

let of_uri dsn =
  try
    of_uri_exn dsn
  with e ->
    None

let of_string_exn dsn =
  Uri.of_string dsn
  |> of_uri_exn

let of_string dsn =
  try
    of_string_exn dsn
  with e ->
    None

let default =
  Sys.getenv "SENTRY_DSN"
  |> Option.bind ~f:of_string

let arg =
  Command.Spec.Arg_type.create of_string

let arg_exn =
  Command.Spec.Arg_type.create of_string_exn

let event_store_uri { uri ; project_id } =
  sprintf "/api/%d/store/" project_id
  |> Uri.with_path uri

let%test_unit "full DSN" =
  let expect =
    make ~uri:(Uri.of_string "https://test.example.com")
      ~public_key:"abcdef"
      ~private_key:"qwerty"
      ~project_id:12345 ()
  in
  List.iter [ of_string ; of_string_exn ] ~f:(fun f ->
    "https://abcdef:qwerty@test.example.com/12345"
    |> f
    |> [%test_result: t] ~expect)

let%test_unit "only public key DSN" =
  let expect =
    make ~uri:(Uri.of_string "https://test.example.com")
      ~public_key:"lkasl"
      ~project_id:56789 ()
  in
  List.iter [ of_string ; of_string_exn ] ~f:(fun f ->
    "https://lkasl@test.example.com/56789"
    |> f
    |> [%test_result: t] ~expect)

let%test_unit "empty DSN no exception" =
  of_string ""
  |> ignore

let%expect_test "empty DSN exception" =
  Util.with_print_exn (fun () ->
    of_string_exn ""
    |> ignore);
  [%expect {| (Failure "Missing required DSN field(s): SCHEME,HOST,PUBLIC_KEY,PROJECT_ID") |}]

let%test_unit "invalid DSN no exception" =
  of_string "https://asdf@example.com/abcd"
  |> ignore

let%expect_test "invalid DSN exception" =
  Util.with_print_exn (fun () ->
    of_string_exn "https://asdf@example.com/abcd"
    |> ignore);
  [%expect {| (Failure "Invalid PROJECT_ID: abcd (should be an integer)") |}]
