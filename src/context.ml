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

let empty ?(max_breadcrumbs=10) () =
  { environment = None
  ; release = None
  ; server_name = None
  ; tags = String.Table.create ()
  ; extra = String.Table.create ()
  ; modules = String.Table.create ()
  ; breadcrumbs = Queue.create ~capacity:max_breadcrumbs () }

let copy t =
  { t with
    tags = String.Table.copy t.tags
  ; extra = String.Table.copy t.extra }

let merge_into_table new_ existing =
  List.iter new_ ~f:(fun (key, data) ->
    Hashtbl.set existing ~key ~data)

let merge_tags tags t =
  merge_into_table tags t.tags

let merge_extra extra t =
  merge_into_table extra t.extra

let merge_modules modules t =
  merge_into_table modules t.modules

let add_breadcrumb crumb t =
  if Queue.capacity t.breadcrumbs = Queue.length t.breadcrumbs then
    Queue.dequeue_exn t.breadcrumbs |> ignore;
  Queue.enqueue t.breadcrumbs crumb

let default_environment = Sys.getenv_opt "SENTRY_ENVIRONMENT"
let default_release = Sys.getenv_opt "SENTRY_RELEASE"

let default_server_name =
  match Sys.getenv_opt "SENTRY_NAME" with
  | None -> Some (Unix.gethostname ())
  | value -> value

let default_extra =
  (* TODO: Add system username (Unix.getlogin) and cwd (Sys.getcwd). We need to
     handle Deferred in here for that to work *)
  [ "os_type", `String Sys.os_type
  ; "executable_name", `String Sys.executable_name
  ; "argv",
    `List (Sys.argv |> Array.to_list |> List.map ~f:(fun v -> `String v))
  ; "backend_type", `String (match Caml.Sys.backend_type with
      | Caml.Sys.Native -> "Native"
      | Bytecode -> "Bytecode"
      | Other o -> o) ]
  |> String.Table.of_alist_exn

let default ?max_breadcrumbs () =
  let empty = empty ?max_breadcrumbs () in
  { empty with
    environment = default_environment
  ; release = default_release
  ; server_name = default_server_name
  ; extra = String.Table.copy default_extra }
