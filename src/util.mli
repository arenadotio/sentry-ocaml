open Core_kernel

(** [cap_string_length ?max_len str] creates a substring of [str] of length
    [max_len] if [str] is longer than [max_len]. Otherwise returns [str]
    unchanged. *)
val cap_string_length : ?max_len:int -> string -> string

(** [empty_list_option l] returns [None] if l is an empty list and returns
    [Some l] otherwise. This is useful for removing empty lists from our JSON
    payloads. *)
val empty_list_option : 'a list -> 'a list option

(** [map_to_alist_option] converts a map to an alist and returns [None] if the
    resulting list is empty or [Some _] if it is not. This is useful for
    converting our maps to the types atdgen expects. *)
val map_to_alist_option : ('key, 'value, _) Map.t -> ('key * 'value) list option

(** [with_print_exn] runs [f] and prints the exception it throws [if
    applicable]. This function is only for testing. *)
val with_print_exn : (unit -> unit) -> unit
