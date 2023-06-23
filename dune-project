(lang dune 2.0)

(name ozulip)

(generate_opam_files true)

(source
 (github OCamlPro/ozulip))

(authors "Mohamed Hernouf <mohamed.hernouf@ocamlpro.com>")

(maintainers "Mohamed Hernouf <mohamed.hernouf@ocamlpro.com>")

(license "LGPL-2.1-only with OCaml-LGPL-linking-exception")

(documentation https://ocamlpro.github.io/ozulip/)

(package
 (name ozulip)
 (synopsis "OCaml bindings to Zulip API")
 (depends
  (ocaml
   (>= 4.10))
  (dune
   (>= 2.0))
  ez_api
  re
  base64
  lwt
  (ez_file (>= 0.3.0))
  cohttp-lwt-unix
  yojson
  logs)
 (tags
  (zulip bindings api)))
