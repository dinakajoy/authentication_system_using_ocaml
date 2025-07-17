(lang dune 3.11)
(name auth_system)
(version 0.1.0)
(generate_opam_files true)

(license MIT)
(authors "Your Name")
(maintainers "you@example.com")
(source (github youruser/auth_system))

(package
 (name auth_system)
 (synopsis "A Dream-based OCaml web app with authentication")
 (description "A simple OCaml web application using Dream, TyXML, Caqti, and secure password hashing via scrypt.")
 (depends
  (ocaml (>= 4.14.0))
  dune
  dream
  dotenv
  yojson
  lwt
  caqti
  caqti-lwt
  caqti-driver-postgresql
  tyxml
  ppx_deriving_yojson
  lwt_ppx
  scrypt
 ))
