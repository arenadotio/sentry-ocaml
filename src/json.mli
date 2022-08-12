(** Wrapper for Yojson.Basic.t with sexp_of *)
type t = Yojson.Basic.t [@@deriving sexp_of]
