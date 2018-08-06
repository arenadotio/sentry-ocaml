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

(** [with_environment dsn f] overrides the default environment tag (from
    the environment variable [SENTRY_ENVIRONMENT]) within the execution of [f]
*)
val with_environment : string -> (unit -> 'a) -> 'a

(** [with_release dsn f] overrides the default release tag (from the environment
    variable [SENTRY_RELEASE]) within the execution of [f] *)
val with_release : string -> (unit -> 'a) -> 'a

(** [with_server_name dsn f] overrides the default server name (default: lookup
    host name) within the execution of [f] *)
val with_server_name : string -> (unit -> 'a) -> 'a

(** [with_tags tags f] merges the given tags with the default tags within the
    execution of [f] *)
val with_tags : (string * string) list -> (unit -> 'a) -> 'a

(** Helper for calling multiple of the [with_*] functions above at once *)
val with_config
  : ?dsn:Dsn.t
  -> ?environment:string
  -> ?release:string
  -> ?server_name:string
  -> ?tags:(string * string) list
  -> (unit -> 'a)
  -> 'a

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

(** [context ?dsn f] runs [f]. If [f] throws an exception, it will be
    uploaded to Sentry and then re-reraised.

    If you pass [tags], it will be as if you called [with_tags] before this
    function. *)
val context : ?tags:(string * string) list -> (unit -> 'a) -> 'a

(** [context_ignore ?dsn f] is like [context] except exceptions will not be
    re-raised. Use this if you're using Sentry in a loop where you want to
    report on errors and then continue (like in an web server).

    If you pass [tags], it will be as if you called [with_tags] before this
    function. *)
val context_ignore : ?tags:(string * string) list -> (unit -> unit) -> unit

(** [context_or_error ?dsn f] runs [f]. If [f] throws an exception or error, it
    will be uploaded to Sentry and then re-raised or returned. Note that
    [Error.t] does not handle backtraces as well as exceptions.

    If you pass [tags], it will be as if you called [with_tags] before this
    function. *)
val context_or_error
  : ?tags:(string * string) list
  -> (unit -> 'a Or_error.t)
  -> 'a Or_error.t

(** [context_async ?dsn f] runs [f]. If [f] throws one or more exceptions, they
    will be uploaded to Sentry. The first raised exception willl be re-raised
    (multiple exceptions could be raised to the Async monitor but only one can
    be re-raised).

    If you pass [tags], it will be as if you called [with_tags] before this
    function. *)
val context_async
  : ?tags:(string * string) list
  -> (unit -> 'a Deferred.t)
  -> 'a Deferred.t

(** See [context_ignore] and [context_async] *)
val context_async_ignore
  : ?tags:(string * string) list
  -> (unit -> unit Deferred.t)
  -> unit Deferred.t

(** [context_async_or_error ?dsn f] runs [f]. If [f] throws an exception or
    returns an error, it will be uploaded to Sentry and then re-raised or
    returned.

    If you pass [tags], it will be as if you called [with_tags] before this
    function. *)
val context_async_or_error
  : ?tags:(string * string) list
  -> (unit -> 'a Deferred.Or_error.t)
  -> 'a Deferred.Or_error.t
