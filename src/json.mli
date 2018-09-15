(** Wrapper for Yojson.Basic.json with sexp_of *)
type t = Yojson.Basic.json [@@deriving sexp_of]
