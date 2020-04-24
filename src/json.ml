type t = Yojson.Basic.json

let sexp_of_t (t : Yojson.Basic.json) =
  Json_derivers.Yojson.sexp_of_t (t :> Json_derivers.Yojson.t)
;;
