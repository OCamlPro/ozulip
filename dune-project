(lang dune 3.4)

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
  ocaml 
  dune
  ezcurl-lwt)
 (tags (zulip bindings api)))
