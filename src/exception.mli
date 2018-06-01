(** https://docs.sentry.io/clientdev/interfaces/exception/ *)
open Core

module Mechanism : sig
  type t = private
    { type_ : string
    ; description : string option
    ; help_link : string option
    ; handled : bool option
    (* TODO: meta *)
    ; data : string String.Map.t }

  val make
    : type_:string
    -> ?description:string
    -> ?help_link:string
    -> ?handled:bool
    -> ?data:string String.Map.t
    -> unit
    -> t 

  val to_payload : t -> Payloads_t.mechanism
end

module Frame : sig
  (** https://docs.sentry.io/clientdev/interfaces/stacktrace/ *)
  type t = private
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
    (* TODO: image_addr, instruction_addr, symbol_addr, instruction_offset *) }

  val make
    : ?filename:string
    -> ?function_:string
    -> ?module_:string
    -> ?lineno:int
    -> ?colno:int
    -> ?abs_path:string
    -> ?context_line:string
    -> ?pre_context:string list
    -> ?post_context:string list
    -> ?in_app:bool
    -> ?vars:string String.Map.t
    -> ?package:string
    -> ?platform:Platform.t
    -> unit
    -> t Or_error.t

  val make_exn
    : ?filename:string
    -> ?function_:string
    -> ?module_:string
    -> ?lineno:int
    -> ?colno:int
    -> ?abs_path:string
    -> ?context_line:string
    -> ?pre_context:string list
    -> ?post_context:string list
    -> ?in_app:bool
    -> ?vars:string String.Map.t
    -> ?package:string
    -> ?platform:Platform.t
    -> unit
    -> t

  val to_payload : t -> Payloads_t.stackframe
end

type t = private
  { type_ : string
  ; value : string
  ; module_ : string option
  ; thread_id : string option
  ; mechanism : Mechanism.t option
  ; stacktrace : Frame.t list }

val make
  : type_:string
  -> value:string
  -> ?module_:string
  -> ?thread_id:string
  -> ?mechanism:Mechanism.t
  -> ?stacktrace:Frame.t list
  -> unit
  -> t

val to_payload : t -> Payloads_t.exception_value

val list_to_payload : t list -> Payloads_t.exception_

val of_exn : exn -> t