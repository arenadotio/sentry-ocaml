(** Atdgen wrapper for the Sentry Datetime type

    The timestamp are in ISO 8601 format, without a timezone.

    https://docs.sentry.io/clientdev/attributes/#required-attributes *)
open Core

type t = Time.t

val wrap : string -> t
val unwrap : t -> string
