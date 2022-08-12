type t = Yojson.Basic.t

let sexp_of_t (t : Yojson.Basic.t) =
  Json_derivers.Yojson.sexp_of_t (t :> Json_derivers.Yojson.t)
;;
