[![CircleCI](https://circleci.com/gh/brendanlong/sentry-ocaml.svg?style=shield)](https://circleci.com/gh/brendanlong/sentry-ocaml)
[![Coverage Status](https://coveralls.io/repos/github/brendanlong/sentry-ocaml/badge.svg?branch=master)](https://coveralls.io/github/brendanlong/sentry-ocaml?branch=master)
[![GitHub release](https://img.shields.io/github/release/brendanlong/sentry-ocaml.svg)](https://github.com/brendanlong/sentry-ocaml/releases/latest)
[![License: Unlicense](https://img.shields.io/badge/license-Unlicense-blue.svg)](http://unlicense.org/)


# Sentry (OCaml) - WORK IN PROGRESS

This is an unofficial work-in-progress [Sentry](https://sentry.io) library for
OCaml.

**This currently requires the Async scheduler to be running or data will not
be uploaded**

## Missing features:

  - Only supports Async (pull requests to factor out Lwt/Unix are welcome!)
  - Global unhandled exception handler isn't implemented yet.
  - Probably other things

## Example

See [the example program](bin/sentry_example.ml).

In general, you should use this like:

```
let () =
  let spec = Command.Spec.(...) in
  Command.async_spec ~summary:"..." spec
  @@ fun args () ->
  (* Using [SENTRY_DSN] from the environment *)
  Sentry.context @@ fun () ->
  (* your normal code here *)
```

**To release the warning above: The Async schedule *must* be running.**

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

## Tags

We upload some data by default. From environment variables, we get:

  - `SENTRY_ENVIRONMENT` -> `environment`
  - `SENTRY_RELEASE` -> `release`

From `Sys` or `Unix`:

  - `Sys.argv` -> `argv`
  - `Sys.backend_type` -> `backend_type`
  - `Sys.executable_name` -> `executable_name`
  - `Sys.gethostname` -> `server_name`
  - `Sys.os_type` -> `os_type`

You can override any of these with `Sentry.with_*` functions, i.e.
`Sentry.with_environment`, `Sentry.with_release`, etc.

You can also upload custom tags using either `Sentry.with_tags` or by passing
`~tags` to a `capture` or `context` function. Tags will be merged for the
current async job, so you only need to pass additional tags:

```
Sentry.context ~tags:[ "app_name", "http_server" ] @@ fun () ->
...
Sentry.with_tags [ "method", "GET", "path", "/example" ] @@ fun () ->
...
(* This will upload with default tags + app_name, method, path, and user_id *)
Sentry.capture_message ~tags:[ "user_id", user_id ] "invalid login"
```
