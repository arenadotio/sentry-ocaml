[![CircleCI](https://circleci.com/gh/brendanlong/sentry-ocaml.svg?style=shield)](https://circleci.com/gh/brendanlong/sentry-ocaml)
[![Coverage Status](https://coveralls.io/repos/github/brendanlong/sentry-ocaml/badge.svg?branch=master)](https://coveralls.io/github/brendanlong/sentry-ocaml?branch=master)

# Sentry (OCaml) - WORK IN PROGRESS

This is an unofficial work-in-progress [Sentry](https://sentry.io) library for
OCaml.

It currently works reasonably well with non-async code (async backtraces are
currently mostly-useless).

## Missing features:

 - Graceful error handling (currently exceptions thrown by Cohttp will be
   re-thrown instead of logged). This is a violation of Sentry's SDK guidelines
   and will be fixed (probably by adding a `~logger` argument).
 - No support for tags, user info, etc. Currently we only upload messages and
   exceptions.
 - Global unhandled exception handler isn't implemented yet.
 - Probably other things

Right now this library is Async-specific but Lwt and synchronous versions are
planned.

## Example

See [the example program](bin/sentry_example.ml).

In general, you should use this like:

```
let () =
  let sentry =
    Uri.of_string "https://[PUBLIC_KEY]@[HOST]/[PROJECT_ID]"
    |> Sentry.of_dsn_exn
  in
  Sentry.context @@ fun () ->
  (* your normal code here *)
```

This will execute your code as usual, and if it throws an exception, it will be
uploaded to Sentry:

![Exception in Sentry](static/exception_in_sentry.png)

Then the exception will be re-thrown so your program will exit and print the
backtrace to stderr as usual (if you want to continue after errors, wrap
`Sentry.context` in another error handler or use `Sentry.context_ignore`).

Note that `Sentry.context_or_error` exists (which handles both exceptions and
`Or_error.t`), but using exceptions exclusively is recommended because they have
backtraces (and wrapping exceptions in `Error.t` loses whatever backtrace did
exist in most cases).