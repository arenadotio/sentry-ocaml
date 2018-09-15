(** A context is a set of tags and breadcrumbs attached to an event
    https://docs.sentry.io/clientdev/context/

    Note that contexts are mutable to make it easier to add tags and
    breadcrumbs. *)
open Core_kernel

type t =
  { mutable environment : string option
  ; mutable release : string option
  ; mutable server_name : string option
  ; tags : string String.Table.t
  ; extra : Json.t String.Table.t
  ; modules : string String.Table.t
  ; breadcrumbs : Breadcrumb.t Queue.t }
[@@deriving sexp_of]

(** Returns a new context with default data from the environment and system
    calls. *)
val default : ?max_breadcrumbs:int -> unit -> t

(** Returns a new context with no tags or breadcrumbs. You probably want to use
    [default ()] or copying the parent context in most cases. *)
val empty : ?max_breadcrumbs:int -> unit -> t

(** Copy a context so you can add / clear data without affecting the original *)
val copy : t -> t

(** [merge_tags tags t] merges the given tags into [t] *)
val merge_tags : (string * string) list -> t -> unit

(** [merge_extra extra t] merges the given extra data into [t] *)
val merge_extra : (string * Json.t) list -> t -> unit

(** [merge_modules modules t] merges the given module info into [t] *)
val merge_modules : (string * string) list -> t -> unit

(** Add a breadcrumb to the context and remove older breadcrumbs *)
val add_breadcrumb : Breadcrumb.t -> t -> unit
