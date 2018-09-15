open Core_kernel
open Util

type t =
  { event_id : Uuidm.t sexp_opaque
  ; timestamp : Time.t sexp_opaque
  ; logger : string sexp_option
  ; platform : Platform.t
  ; sdk : Sdk.t
  ; level : Severity_level.t sexp_option
  ; culprit : string sexp_option
  ; server_name : string sexp_option
  ; release : string sexp_option
  ; tags : string String.Map.t
  ; environment : string sexp_option
  ; modules : string String.Map.t
  ; extra : Json.t String.Map.t
  ; fingerprint : string list sexp_option
  ; exception_ : Exception.t list option sexp_opaque
  ; message : Message.t sexp_option
  ; breadcrumbs : Breadcrumb.t list }
[@@deriving sexp_of]

let make ?event_id ?timestamp ?context ?tags ?logger
      ?(platform=`Other) ?(sdk=Sdk.default) ?level ?culprit ?fingerprint
      ?message ?exn () =
  let { Context.server_name ; release ; environment ; extra
      ; tags = context_tags ; breadcrumbs } =
    match context with
    | Some context -> context
    | None -> Context.empty ()
  in
  let tags =
    begin match tags with
    | None
    | Some [] -> context_tags
    | Some tags ->
      let merged = Hashtbl.copy context_tags in
      List.iter tags ~f:(fun (key, data) ->
        Hashtbl.set merged ~key ~data);
      merged
    end
    |> Hashtbl.to_alist
    |> String.Map.of_alist_exn
  in
  let event_id =
    match event_id with
    | Some id -> id
    | None -> Uuidm.create `V4
  in
  let timestamp =
    match timestamp with
    | Some ts -> ts
    | None -> Time.now ()
  in
  { event_id ; timestamp ; logger ; platform ; sdk ; level ; culprit
  ; server_name ; release ; tags ; environment ; modules = String.Map.empty
  ; extra = String.Table.to_alist extra |> String.Map.of_alist_exn
  ; fingerprint ; message ; exception_ = exn
  ; breadcrumbs = Queue.to_list breadcrumbs }

let to_payload { event_id ; timestamp ; logger ; platform ; sdk ; level
               ; culprit ; server_name ; release ; tags ; environment ; modules
               ; extra ; fingerprint ; exception_ ; message ; breadcrumbs } =
  { Payloads_t.event_id ; timestamp ; logger ; platform
  ; sdk = Sdk.to_payload sdk
  ; level ; culprit ; server_name ; release
  ; tags = map_to_alist_option tags
  ; environment
  ; modules = map_to_alist_option modules
  ; extra = map_to_alist_option extra
  ; fingerprint
  ; exception_ = Option.map ~f:Exception.list_to_payload exception_
  ; message = Option.map ~f:Message.to_payload message
  ; breadcrumbs = (match breadcrumbs with
      | [] -> None
      | _ -> Some (List.map breadcrumbs ~f:Breadcrumb.to_payload)) }

let to_json_string t =
  to_payload t
  |> Payloads_j.string_of_event

let%expect_test "to_json_string basic" =
  let event_id = Uuid.wrap "bce345569e7548a384bac4512a9ad909" in
  let timestamp = Time.of_string "2018-08-03T11:44:21.298019Z" in
  make ~event_id ~timestamp ()
  |> to_json_string
  |> print_endline;
  [%expect {| {"event_id":"bce345569e7548a384bac4512a9ad909","timestamp":"2018-08-03T11:44:21.298019","platform":"other","sdk":{"name":"sentry-ocaml","version":"0.1"}} |}]

let%expect_test "to_json_string everything" =
  begin try
    raise (Failure "test")
  with exn ->
    let event_id = Uuid.wrap "ad2579b4f62f486498781636c1450148" in
    let timestamp = Time.of_string "2014-12-23T22:44:21.2309Z" in
    let message = Message.make ~message:"Testy test test" () in
    let context = Context.empty () in
    context.server_name <- Some "example.com";
    context.release <- Some "5";
    context.environment <- Some "dev";
    Context.merge_modules [ "ocaml", "4.02.1" ; "core", "v0.10" ] context;
    Context.merge_extra [ "a thing", `String "value" ] context;
    Breadcrumb.make ~timestamp ~message:"test crumb" ()
    |> Fn.flip Context.add_breadcrumb context;

    make ~event_id ~timestamp ~logger:"test" ~platform:`Python ~context
      ~sdk:(Sdk.make ~name:"test-sdk" ~version:"10.5" ()) ~level:`Error
      ~culprit:"the tests"
      ~tags:[ "a", "b" ; "c", "d" ]
      ~fingerprint:["039432409" ; "asdf"]
      ~message ~exn:[Exception.of_exn exn] ()
    |> to_json_string
    |> print_endline
  end;
  [%expect {| {"event_id":"ad2579b4f62f486498781636c1450148","timestamp":"2014-12-23T22:44:21.230900","logger":"test","platform":"python","sdk":{"name":"test-sdk","version":"10.5"},"level":"error","culprit":"the tests","server_name":"example.com","release":"5","tags":{"a":"b","c":"d"},"environment":"dev","extra":{"a thing":"value"},"fingerprint":["039432409","asdf"],"exception":{"values":[{"type":"Failure","value":"test","stacktrace":{"frames":[{"filename":"src/event.ml","lineno":93,"colno":4}]}}]},"sentry.interfaces.Message":{"message":"Testy test test"},"breadcrumbs":[{"timestamp":"2014-12-23T22:44:21.230900","type":"default","message":"test crumb","data":{},"level":"info"}]} |}]
