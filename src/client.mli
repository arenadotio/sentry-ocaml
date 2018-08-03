open Core_kernel
open Async_kernel

(** Low level functions to access the Sentry API. You probably want the high
    level functions in sentry.ml *)

(** [send_event ~dsn message] uploads a message to Sentry using the given
    [dsn]. Uploading happens in the background but will finish before the
    program exits. *)
val send_event : dsn:Dsn.t' -> Event.t -> unit

(** [send_event_and_wait] immediately uploads a message to Sentry and waits for
    the upload to complete. Returns the UUID of the created event or [None] if
    an exception occurred. *)
val send_event_and_wait : dsn:Dsn.t' -> Event.t -> Uuidm.t option Deferred.t

(** Like [send_event_and_wait] but throws exceptions if uploading fails. *)
val send_event_and_wait_exn : dsn:Dsn.t' -> Event.t -> Uuidm.t Deferred.t