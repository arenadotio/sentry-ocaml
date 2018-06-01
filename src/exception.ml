open Core
open Util

module Mechanism = struct
  type t =
    { type_ : string
    ; description : string option
    ; help_link : string option
    ; handled : bool option
    (* TODO: meta *)
    ; data : string String.Map.t }

  let make ~type_ ?description ?help_link ?handled ?(data=String.Map.empty)
        () =
    { type_ ; description ; help_link ; handled ; data }

  let to_payload { type_ ; description ; help_link ; handled ; data } =
    { Payloads_t.type_ ; description ; help_link ; handled
    ; data = map_to_alist_option data }
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
    ; pre_context : string list option
    ; post_context : string list option
    ; in_app : bool option
    ; vars : string String.Map.t
    ; package : string option
    ; platform : Platform.t option
    (* TODO: image_addr, instruction_addr, symbol_addr, instruction_offset *) }

  let make ?filename ?function_ ?module_ ?lineno ?colno ?abs_path
        ?context_line ?pre_context ?post_context ?in_app
        ?(vars=String.Map.empty) ?package ?platform () =
    [ filename ; function_ ; module_ ]
    |> List.for_all ~f:Option.is_none
    |> function
    | true ->
      Or_error.error_string "One of filename, function_ or module_ is \
                             required in Frame.make"
    | false ->
      Ok { filename
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
         ; platform }

  let make_exn ?filename ?function_ ?module_ ?lineno ?colno ?abs_path
        ?context_line ?pre_context ?post_context ?in_app
        ?vars ?package ?platform () =
    make ?filename ?function_ ?module_ ?lineno ?colno ?abs_path
      ?context_line ?pre_context ?post_context ?in_app
      ?vars ?package ?platform ()
    |> Or_error.ok_exn

  let to_payload { filename ; function_ ; module_ ; lineno ; colno
                 ; abs_path ; context_line ; pre_context ; post_context
                 ; in_app ; vars ; package ; platform } =
    { Payloads_t.filename ; function_ ; module_ ; lineno ; colno ; abs_path
    ; context_line ; pre_context ; post_context ; in_app
    ; vars = map_to_alist_option vars
    ; package ; platform }
end

type exception_ =
  { type_ : string
  ; value : string
  ; module_ : string option
  ; thread_id : string option
  ; mechanism : Mechanism.t option
  ; stacktrace : Frame.t list }

type t = exception_ list

let make ~type_ ~value ?module_ ?thread_id ?mechanism ?(stacktrace=[]) () =
  { type_ ; value ; module_ ; thread_id ; mechanism ; stacktrace }

let to_payload { type_ ; value ; module_ ; thread_id ; mechanism
               ; stacktrace } =
  { Payloads_t.type_ ; value ; module_ ; thread_id
  ; mechanism = Option.map mechanism ~f:Mechanism.to_payload
  ; stacktrace =
      List.map stacktrace ~f:Frame.to_payload
      |> empty_list_option
      |> Option.map ~f:(fun frames ->
        { Payloads_t.frames }) }

let list_to_payload t =
  let values = List.map t ~f:to_payload in
  { Payloads_t.values }

let of_exn exn =
  let type_ = Caml.Printexc.exn_slot_name exn in
  let value = Caml.Printexc.to_string exn in
  let stacktrace =
    Caml.Printexc.get_raw_backtrace ()
    |> Caml.Printexc.backtrace_slots
    |> Option.value ~default:[||]
    |> Array.to_list
    |> List.filter_map ~f:(fun frame ->
      match Caml.Printexc.Slot.location frame with
      | None -> None
      | Some { Caml.Printexc.filename ; line_number ; start_char
             ; end_char } ->
        Frame.make ~filename ~lineno:line_number ~colno:start_char ()
        |> Option.some)
    |> Or_error.all
    (* Asserting that there are no errors here since we always pass
       ~filename to Frame.make *)
    |> Or_error.ok_exn
  in
  make ~type_ ~value ~stacktrace ()