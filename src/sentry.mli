open Core_kernel
open Async_kernel

module Client = Client
module Config = Config
module Dsn = Dsn
module Event = Event
module Exception = Exception
module Platform = Platform
module Sdk = Sdk
module Severity_level = Severity_level

(** [with_dsn dsn f] overrides the default DSN (from the environment variable
    [SENTRY_DSN]) within the execution of [f] *)
val with_dsn : Dsn.t -> (unit -> 'a) -> 'a

(** [with_context f] sets the current thread-local context and runs [f] with
    it. *)
val with_context : Context.t -> (unit -> 'a) -> 'a

(** Like [with_new_context] but creates the new context for you as a copy
    of the current context. *)
val with_new_context : (Context.t -> 'a) -> 'a

(** Override the environment in the current context *)
val set_environment : string -> unit

(** Override the release in the current context *)
val set_release : string -> unit

(** Override the server name in the current context *)
val set_server_name : string -> unit

(** Merge tags into the current context *)
val merge_tags : (string * string) list -> unit

(** [capture_message ?tags ?dsn message] uploads a message to Sentry using the
    given [dsn] (or looking it up in the environment).

    If you pass [tags], it will be as if you called [with_tags] before this
    function. *)
val capture_message : ?tags:(string * string) list -> string -> unit

(** [capture_exception ?dsn ?message e] records the backtrace from [e] and an
    optional message and uploads it to Sentry.

    If you pass [tags], it will be as if you called [with_tags] before this
    function. *)
val capture_exception
  : ?tags:(string * string) list
  -> ?message:string
  -> exn
  -> unit

(** [capture_error ?dsn ?message e] records the backtrace from [e] and uploads
    it to Sentry.

    If you pass [tags], it will be as if you called [with_tags] before this
    function. *)
val capture_error : ?tags:(string * string) list -> Error.t -> unit

(** [with_exn_handler ?dsn f] runs [f]. If [f] throws an exception, it will be
    uploaded to Sentry and then re-reraised. *)
val with_exn_handler : (unit -> 'a) -> 'a

(** [with_exn_handler ?dsn f] is like [context] except exceptions will not be
    re-raised. Use this if you're using Sentry in a loop where you want to
    report on errors and then continue (like in an web server). *)
val with_exn_handler_ignore : (unit -> unit) -> unit

(** [with_error_and_exn_handler ?dsn f] runs [f]. If [f] throws an exception or
    error, it will be uploaded to Sentry and then re-raised or returned. Note
    that [Error.t] does not handle backtraces as well as exceptions. *)
val with_error_and_exn_handler : (unit -> 'a Or_error.t) -> 'a Or_error.t

(** [with_async_exn_handler f] runs [f]. If [f] throws one or more exceptions, they
    will be uploaded to Sentry. The first raised exception willl be re-raised
    (multiple exceptions could be raised to the Async monitor but only one can
    be re-raised). *)
val with_async_exn_handler : (unit -> 'a Deferred.t) -> 'a Deferred.t

(** See [with_exn_handler_ignore] and [with_async_exn_handler] *)
val with_async_exn_handler_ignore : (unit -> unit Deferred.t) -> unit Deferred.t

(** [with_async_error_and_exn_handler f] runs [f]. If [f] throws an exception or
    returns an error, it will be uploaded to Sentry and then re-raised or
    returned. *)
val with_async_error_and_exn_handler
  : (unit -> 'a Deferred.Or_error.t)
  -> 'a Deferred.Or_error.t
