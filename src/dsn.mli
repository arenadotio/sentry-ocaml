open Core

type t' = private
  { uri : Uri_sexp.t
  ; public_key : string
  ; private_key : string option
  ; project_id : int }
[@@deriving sexp_of]

type t = t' option [@@deriving sexp_of]

val make
  : uri:Uri.t
  -> public_key:string
  -> ?private_key:string
  -> project_id:int
  -> unit
  -> t

(** [default] is the DSN determined from [SENTRY_DSN] in the environment (or
    [None] if it's not set or invalid) *)
val default : t

(** [of_string dsn] parses the given DSN and returns the resulting Sentry
    config. You should generally not use this function directly and should use
    [Sentry.context] (or [Sentry.async_context]), which looks up the DSN for
    you from the environment.

    The DSN should look like
    ['{PROTOCOL}://{PUBLIC_KEY}:{PRIVATE_KEY}@{HOST}/{PATH}{PROJECT_ID}']

    All values except for [PRIVATE_KEY] are required. [PROJECT_ID] must be an
    integer. In general you should get this value from Sentry and should not
    construct it yourself.

    Returns [None] if the DSN is invalid.

    See docs: https://docs.sentry.io/quickstart/#about-the-dsn *)
val of_string : string -> t

(** [of_uri dsn] is like [of_string dsn] but takes a [Uri.t] *)
val of_uri : Uri.t -> t

(** [arg] is a Command.Spec argument type which calls [of_string] *)
val arg : t Command.Arg_type.t

(** Like [arg] but uses [of_string_exn] *)
val arg_exn : t Command.Arg_type.t

(** [event_store_uri] is the URI we should POST Sentry events to *)
val event_store_uri : t' -> Uri.t
