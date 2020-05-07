(lang dune 1.11)

(using fmt 1.1)

(name sentry)

(generate_opam_files true)

(license "Unlicense")

(maintainers "Brendan Long <self@brendanlong.com>")

(authors "Brendan Long <self@brendanlong.com>")

(source
 (github brendanlong/sentry-ocaml))

(documentation "https://brendanlong.github.io/sentry-ocaml")

(package
 (name sentry)
 (synopsis "Unofficial Async Sentry error monitoring client")
 (description
  "Sentry is an unofficial Async OCaml client for the Sentry error reporting.")
 (depends
  (async_unix (>= v0.13.0))
  atdgen
  (bisect_ppx (and :dev (>= 2.0.0)))
  (cohttp (>= 2.0.0))
  (cohttp-async (>= 2.0.0))
  (dune (>= 1.11.0))
  (hex (>= 1.2.0))
  json-derivers
  ppx_jane
  (ocaml (>= 4.08.0))
  re2
  (sexplib (>= v0.13.0))
  uuidm
  uri
  yojson))
