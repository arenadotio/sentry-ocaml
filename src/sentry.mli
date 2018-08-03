open Core_kernel
open Async_kernel

module Config = Config
module Event = Event
module Exception = Exception
module Platform = Platform
module Sdk = Sdk
module Severity_level = Severity_level

type t' = private
  { uri : Uri.t
  ; public_key : string
  ; private_key : string option
  ; project_id : int
  ; event_pipe : Event.t Pipe.Writer.t }
[@@deriving sexp_of]

type t = t' option [@@deriving sexp_of]

(** [empty] is a Sentry config that ignores everything sent to it. This can
    be useful in development environments. *)
val empty : t

(** [of_dsn dsn] parses the given DSN and returns the resulting Sentry config.

    The DSN should look like
    ['{PROTOCOL}://{PUBLIC_KEY}:{PRIVATE_KEY}@{HOST}/{PATH}{PROJECT_ID}']

    All values except for [PRIVATE_KEY] are required. [PROJECT_ID] must be an
    integer. In general you should get this value from Sentry and should not
    construct it yourself.

    If the DSN is empty, [empty] will be returned and all errors will be
    ignored.

    See docs: https://docs.sentry.io/quickstart/#about-the-dsn *)
val of_dsn : Uri.t -> t

(** Like [of_dsn] but raises an exception if the DSN is invalid. *)
val of_dsn_exn : Uri.t -> t

(** [capture_message t message] uploads a message to Sentry. *)
val capture_message : t -> string -> unit

(** [capture_exception t ?message e] records the backtrace from [e] and an
    optional message and uploads it to Sentry. *)
val capture_exception : t -> ?message:string -> exn -> unit

(** [capture_error t ?message e] records the backtrace from [e] and uploads it
    to Sentry. *)
val capture_error : t -> Error.t -> unit

(** [context t f] runs [f]. If [f] throws an exception, it will be
    uploaded to Sentry and then re-reraised. *)
val context : t -> (unit -> 'a) -> 'a

(** [context_ignore t f] is like [context] except exceptions will not be
    re-raised. Use this if you're using Sentry in a loop where you want to
    report on errors and then continue (like in an web server). *)
val context_ignore : t -> (unit -> unit) -> unit

(** [context_or_error t f] runs [f]. If [f] throws an exception or error, it
    will be uploaded to Sentry and then re-raised or returned. Note that
    [Error.t] does not handle backtraces as well as exceptions. *)
val context_or_error : t -> (unit -> 'a Or_error.t) -> 'a Or_error.t

(** [context_async t f] runs [f]. If [f] throws one or more exceptions, they will be
    uploaded to Sentry. The first raised exception willl be re-raised (multiple
    exceptions could be raised to the Async monitor but only one can be
    re-raised). *)
val context_async : t -> (unit -> 'a Deferred.t) -> 'a Deferred.t

(** See [context_ignore] and [context_async] *)
val context_async_ignore : t -> (unit -> unit Deferred.t) -> unit Deferred.t

(** [context_async_or_error t f] runs [f]. If [f] throws an exception or returns
    an error, it will be uploaded to Sentry and then re-raised or returned. *)
val context_async_or_error : t -> (unit -> 'a Deferred.Or_error.t) -> 'a Deferred.Or_error.t