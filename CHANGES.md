## v0.10.1

- Don't require bisect_ppx for builds

## v0.10.0

- Removed unused dependencies like async_extended and ounit
- Better handing of 429 errors (#27)

## v0.9.0

- Update Core/Async to v0.13
- Update cohttp to v2.0
- Update to bisect_ppx 2.0

## v0.8.0

- Allow messages and exception values to be up to 4KB long

## v0.7.0

- Switch back to cohttp 1.2 since 2.0 is incompatible with core_extended.base64
- Cap strings at 512 characters each to try to keep us under Sentry's API limits

## v0.6.1

- Remove yojson version restrictions again

## v0.6.0

- Support cohttp 2.0
- Require yojson <= 1.5.0
- Add helpers to make esy builds work

## v0.5.0

- Fix an issue where exceptions containing functions cause this library to throw exceptions

## v0.5.5

- Support Uri 2.0 (split into Uri.t and Uri_sexp.t -- we just use a custom sexp function)

## v0.5.1

- Add missing / at the end of Sentry event uploading URL

## v0.4.1

- Add missing / at the end of Sentry event uploading URL

## v0.5

- Redo context to be mutable to match other Sentry libraries and make it easier to add new tags to an existing context
- Add support for breadcrumbs

## v0.4

- Put context/context_async tags in the thread-global map so sub-contexts can use them

## v0.3

- Parse Monitor exceptions to get significantly better async backtraces

## v0.2

- Parse stringified exceptions for much nicer messages
  - Don't print the exception name twice
  - Clean up exception names
  - Print exception messages that are just strings as just a string instead of an sexp of a string (or an sexp of a list of strings)
- Improve examples and README

## v0.1

Initial release
