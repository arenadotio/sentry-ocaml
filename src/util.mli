open Core_kernel

(** [empty_list_option l] returns [None] if l is an empty list and returns
    [Some l] otherwise. This is useful for removing empty lists from our JSON
    payloads. *)
val empty_list_option : 'a list -> 'a list option

(** [map_to_alist_option] converts a map to an alist and returns [None] if the
    resulting list is empty or [Some _] if it is not. This is useful for
    converting our maps to the types atdgen expects. *)
val map_to_alist_option : ('key, 'value, _) Map.t -> ('key * 'value) list option