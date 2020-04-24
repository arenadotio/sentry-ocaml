open Core_kernel
open Async
open Util

module Mechanism = struct
  type t =
    { type_ : string
    ; description : string option
    ; help_link : string option
    ; handled : bool option (* TODO: meta *)
    ; data : string String.Map.t
    }

  let make ~type_ ?description ?help_link ?handled ?(data = String.Map.empty) () =
    { type_; description; help_link; handled; data }
  ;;

  let to_payload { type_; description; help_link; handled; data } =
    { Payloads_t.type_; description; help_link; handled; data = map_to_alist_option data }
  ;;
end

module Frame = struct
  type t =
    { filename : string option
    ; function_ : string option
    ; module_ : string option
    ; lineno : int option
    ; colno : int option
    ; abs_path : string option
    ; context_line : string option
    ; pre_context : string list
    ; post_context : string list
    ; in_app : bool option
    ; vars : string String.Map.t
    ; package : string option
    ; platform : Platform.t option
          (* TODO: image_addr, instruction_addr, symbol_addr, instruction_offset *)
    }

  let make
      ?filename
      ?function_
      ?module_
      ?lineno
      ?colno
      ?abs_path
      ?context_line
      ?(pre_context = [])
      ?(post_context = [])
      ?in_app
      ?(vars = String.Map.empty)
      ?package
      ?platform
      ()
    =
    [ filename; function_; module_ ]
    |> List.for_all ~f:Option.is_none
    |> function
    | true ->
      Or_error.error_string
        "One of filename, function_ or module_ is required in Frame.make"
    | false ->
      Ok
        { filename
        ; function_
        ; module_
        ; lineno
        ; colno
        ; abs_path
        ; context_line
        ; pre_context
        ; post_context
        ; in_app
        ; vars
        ; package
        ; platform
        }
  ;;

  let make_exn
      ?filename
      ?function_
      ?module_
      ?lineno
      ?colno
      ?abs_path
      ?context_line
      ?pre_context
      ?post_context
      ?in_app
      ?vars
      ?package
      ?platform
      ()
    =
    make
      ?filename
      ?function_
      ?module_
      ?lineno
      ?colno
      ?abs_path
      ?context_line
      ?pre_context
      ?post_context
      ?in_app
      ?vars
      ?package
      ?platform
      ()
    |> Or_error.ok_exn
  ;;

  let to_payload
      { filename
      ; function_
      ; module_
      ; lineno
      ; colno
      ; abs_path
      ; context_line
      ; pre_context
      ; post_context
      ; in_app
      ; vars
      ; package
      ; platform
      }
    =
    { Payloads_t.filename
    ; function_
    ; module_
    ; lineno
    ; colno
    ; abs_path
    ; context_line
    ; pre_context = empty_list_option pre_context
    ; post_context = empty_list_option post_context
    ; in_app
    ; vars = map_to_alist_option vars
    ; package
    ; platform
    }
  ;;
end

type t =
  { type_ : string
  ; value : string option
  ; module_ : string option
  ; thread_id : string option
  ; mechanism : Mechanism.t option
  ; stacktrace : Frame.t list
  }

let make ~type_ ?value ?module_ ?thread_id ?mechanism ?(stacktrace = []) () =
  { type_; value; module_; thread_id; mechanism; stacktrace }
;;

let to_payload { type_; value; module_; thread_id; mechanism; stacktrace } =
  { Payloads_t.type_
  ; value
  ; module_
  ; thread_id
  ; mechanism = Option.map mechanism ~f:Mechanism.to_payload
  ; stacktrace =
      List.map stacktrace ~f:Frame.to_payload
      |> empty_list_option
      |> Option.map ~f:(fun frames -> { Payloads_t.frames })
  }
;;

let list_to_payload t =
  let values = List.map t ~f:to_payload in
  { Payloads_t.values }
;;

let backtrace_regex =
  Re2.create_exn
    {|(Raised at|Called from) file "([^"]*)", line ([0-9]+), characters ([0-9]+)-[0-9]+|}
;;

let of_exn exn =
  let stacktrace =
    Caml.Printexc.get_raw_backtrace ()
    |> Caml.Printexc.backtrace_slots
    |> Option.value ~default:[||]
    |> Array.to_list
    (* Frames should be sorted from oldest to newest. *)
    |> List.rev
    |> List.filter_map ~f:(fun frame ->
           match Caml.Printexc.Slot.location frame with
           | None -> None
           | Some { Caml.Printexc.filename; line_number; start_char; _ } ->
             Frame.make ~filename ~lineno:line_number ~colno:start_char () |> Option.some)
    |> Or_error.all
    (* Asserting that there are no errors here since we always pass
       ~filename to Frame.make *)
    |> Or_error.ok_exn
  in
  (* Extract the inner exception for messages and stack trace when dealing
     with async *)
  let base_exn = Monitor.extract_exn exn in
  let type_ =
    (* exn_slot_name prints something like Module__filename.Submodule.Exn_name,
       but we only want Exn_name *)
    Caml.Printexc.exn_slot_name base_exn |> String.split ~on:'.' |> List.last_exn
  in
  let value =
    let str = Caml.Printexc.to_string base_exn in
    (* Try to extract nicer info from the string output *)
    try
      (try Sexp.of_string str with
      | _ ->
        (* Try to parse the default format: Exception_name(arg1, arg2) *)
        String.chop_suffix_exn str ~suffix:")"
        |> String.split ~on:'('
        |> (function
        | name :: args ->
          let args =
            String.concat ~sep:"" args
            |> String.split ~on:','
            |> List.map ~f:(fun arg -> Sexp.of_string arg)
          in
          Sexp.List (Atom name :: args)
        | _ -> assert false))
      |> function
      (* Exceptions using [@@deriving sexp_of] will be in the form
           (Exception_name "message" other args) *)
      | Sexp.List (Atom name :: msgs) when String.is_suffix ~suffix:type_ name ->
        (match msgs with
        | [] -> None
        | [ Atom msg ] -> Some msg
        | sexp -> Sexp.to_string_hum (Sexp.List sexp) |> Option.some)
      (* Handles argumentless exceptions like Not_found *)
      | Atom name when String.is_suffix ~suffix:type_ name -> None
      | sexp -> Sexp.to_string_hum sexp |> Option.some
    with
    | _ -> Some str
  in
  (* Combine the stack trace from the Monitor exn if applicable *)
  let stacktrace, value =
    if phys_equal base_exn exn
    then stacktrace, value
    else (
      let monitor_trace =
        let open Sexp in
        Exn.sexp_of_t exn
        |> function
        | List [ Atom "monitor.ml.Error"; _base_exn; List monitor_stacktrace ] ->
          List.filter_map monitor_stacktrace ~f:(function
              | Atom frame ->
                Re2.find_submatches backtrace_regex frame
                |> (function
                | Ok [| _; _; Some filename; Some lineno; Some colno |] ->
                  (try
                     let lineno = Int.of_string lineno
                     and colno = Int.of_string colno in
                     Frame.make_exn ~filename ~lineno ~colno () |> Option.some
                   with
                  | _ -> None)
                | _ -> None)
              | _ -> None)
        | _ -> []
      in
      match monitor_trace with
      | [] ->
        ( stacktrace
        , List.filter_opt [ value; Some (Exn.to_string exn) ]
          |> String.concat ~sep:"\n\n"
          |> Option.some )
      | monitor_trace -> stacktrace @ monitor_trace, value)
  in
  make ~type_ ?value ~stacktrace ()
;;

let of_error err =
  let open Error.Internal_repr in
  let rec find_backtrace = function
    | With_backtrace (_, bt) -> Some bt
    | Tag_t (_, t) | Tag_arg (_, _, t) -> find_backtrace t
    | Of_list (_, l) -> List.find_map l ~f:find_backtrace
    | _ -> None
  in
  match of_info err with
  | Exn exn -> of_exn exn
  | info ->
    let _backtrace = find_backtrace info in
    (* TODO: Parse backtrace *)
    let type_ = "Error" in
    let value = Error.to_string_hum err in
    make ~type_ ~value ()
;;

let exn_test_helper e =
  try raise e with
  | e ->
    let e = of_exn e in
    { e with
      stacktrace =
        List.map e.stacktrace ~f:(fun frame ->
            { frame with Frame.lineno = Some 192; colno = Some 4 })
    }
    |> to_payload
    |> Payloads_j.string_of_exception_value
    |> print_endline
;;

exception Exception_containing_function of (unit -> unit)

let%expect_test "don't throw compare error on exn containing function" =
  exn_test_helper (Exception_containing_function Fn.id);
  [%expect
    {| {"type":"Exception_containing_function","value":"_","stacktrace":{"frames":[{"filename":"src/exception.ml","lineno":192,"colno":4}]}} |}]
;;

let%expect_test "parse exn to payload" =
  exn_test_helper (Failure "This is a test");
  [%expect
    {| {"type":"Failure","value":"This is a test","stacktrace":{"frames":[{"filename":"src/exception.ml","lineno":192,"colno":4}]}} |}]
;;

let%expect_test "parse Not_found to payload" =
  exn_test_helper Caml.Not_found;
  [%expect
    {| {"type":"Not_found","stacktrace":{"frames":[{"filename":"src/exception.ml","lineno":192,"colno":4}]}} |}]
;;

exception Custom_sexp_exception of string * int list [@@deriving sexp_of]

let%expect_test "parse complex sexp exn to payload" =
  exn_test_helper (Custom_sexp_exception ("This is a test", [ 4; 2 ]));
  [%expect
    {| {"type":"Custom_sexp_exception","value":"(\"This is a test\" (4 2))","stacktrace":{"frames":[{"filename":"src/exception.ml","lineno":192,"colno":4}]}} |}]
;;

exception Custom_no_sexp_exception of string * int list

let%expect_test "parse complex no-sexp exn to payload" =
  exn_test_helper (Custom_no_sexp_exception ("This is a test", [ 4; 2 ]));
  [%expect
    {| {"type":"Custom_no_sexp_exception","value":"(\"This is a test\" _)","stacktrace":{"frames":[{"filename":"src/exception.ml","lineno":192,"colno":4}]}} |}]
;;

exception Custom_no_sexp_single_arg_exception of string

let%expect_test "parse single arg no-sexp exn to payload" =
  exn_test_helper (Custom_no_sexp_single_arg_exception "This is a test");
  [%expect
    {| {"type":"Custom_no_sexp_single_arg_exception","value":"This is a test","stacktrace":{"frames":[{"filename":"src/exception.ml","lineno":192,"colno":4}]}} |}]
;;

exception Custom_no_sexp_no_arg_exception

let%expect_test "parse no arg no-sexp exn to payload" =
  exn_test_helper Custom_no_sexp_no_arg_exception;
  [%expect
    {| {"type":"Custom_no_sexp_no_arg_exception","stacktrace":{"frames":[{"filename":"src/exception.ml","lineno":192,"colno":4}]}} |}]
;;

let%expect_test "parse Error.t to payload" =
  Error.of_string "This is different test"
  |> of_error
  |> to_payload
  |> Payloads_j.string_of_exception_value
  |> print_endline;
  [%expect {|  {"type":"Error","value":"This is different test"} |}]
;;

let%expect_test "parse Async Monitor exception" =
  Backtrace.elide := false;
  Monitor.try_with ~extract_exn:false (fun () -> raise Caml.Not_found)
  >>= fun res ->
  (match res with
  | Ok () -> assert false
  | Error exn -> exn_test_helper exn);
  [%expect
    {| {"type":"Not_found","stacktrace":{"frames":[{"filename":"src/exception.ml","lineno":192,"colno":4},{"filename":"src/exception.ml","lineno":192,"colno":4},{"filename":"src/monitor.ml","lineno":192,"colno":4},{"filename":"src/job_queue.ml","lineno":192,"colno":4}]}} |}]
;;

let%expect_test "parse Async Monitor exception parse failure" =
  Backtrace.elide := true;
  Monitor.try_with ~extract_exn:false (fun () -> raise Caml.Not_found)
  >>= fun res ->
  (match res with
  | Ok () -> assert false
  | Error exn -> exn_test_helper exn);
  [%expect
    {| {"type":"Not_found","value":"(monitor.ml.Error Not_found (\"<backtrace elided in test>\"))","stacktrace":{"frames":[{"filename":"src/exception.ml","lineno":192,"colno":4}]}} |}]
;;
