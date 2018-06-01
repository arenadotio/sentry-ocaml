open Core
open Async

type t' = private
  { uri : Uri.t
  ; public_key : string
  ; private_key : string option
  ; project_id : int }
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
val of_dsn : Uri.t -> t Or_error.t

(** Like [of_dsn] but raises an exception if the DSN is invalid. *)
val of_dsn_exn : Uri.t -> t

(** [capture_message t message] uploads a message to Sentry. *)
val capture_message : t -> string -> Uuidm.t option Deferred.t

(** [capture_exception t ?message e] records the backtrace from [e] and an
    optional message and uploads it to Sentry. *)
val capture_exception : t -> ?message:string -> exn -> Uuidm.t option Deferred.t

(** [context t f] runs [f]. If [f] throws one or more exception, they will be
    uploaded to Sentry. The first raised exception willl be re-raised (multiple
    exceptions could be raised to the Async monitor but only one can be
    re-raised). *)
val context : t -> (unit -> 'a Deferred.t) -> 'a Deferred.t