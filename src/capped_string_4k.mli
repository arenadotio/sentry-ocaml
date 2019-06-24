(** The Sentry API discards anything after the first 512 characters of most
    string attributes, so avoid sending them in the first place, so we can
    also avoid sending events that are too big.
    https://docs.sentry.io/accounts/quotas/#attributes-limits *)
type t = string [@@deriving sexp_of]

val wrap : string -> t
val unwrap : t -> string
