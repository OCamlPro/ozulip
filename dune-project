(lang dune 3.0)

(name ozulip)

(generate_opam_files true)

(implicit_transitive_deps false)

(source
 (github OCamlPro/ozulip))

(authors "Mohamed Hernouf <mohamed.hernouf@ocamlpro.com>")

(maintainers "Dario Pinto <dario.pinto@ocamlpro.com>")

(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")

(documentation https://ocamlpro.github.io/ozulip/)

(package
 (name ozulip)
 (synopsis "OCaml bindings to Zulip API")
 (depends
  (ocaml
   (>= 4.10))
  (dune
   (>= 2.7))
  (ez_api
   (>= 2.0.0))
  re
  base64
  (ez_file
   (>= 0.3.0))
  cohttp-lwt-unix
  (json-data-encoding
   (>= 1.0.0))
  logs
  (lwt
   (>= 5.4.0))
  yojson
  logs)
 (tags
  (zulip bindings api)))
