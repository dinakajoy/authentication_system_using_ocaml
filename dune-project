(lang dune 3.11)
(name auth_system)
(version 0.1.0)
(generate_opam_files true)

(license MIT)
(authors "Odinaka Joy")
(maintainers "dinakajoy@gmail.com")
(source (github dinakajoy/authentication_system_using_ocaml ))

(package
 (name auth_system)
 (synopsis "A Dream-based OCaml web app with authentication")
 (description "A simple OCaml web application using Dream, TyXML, Caqti, and secure password hashing via argon2.")
 (depends
  (ocaml (>= 5.4.0))
  dune
  dream
  cohttp-lwt-unix
  yojson
  lwt
  caqti
  caqti-lwt
  caqti-driver-postgresql
  tyxml
  ppx_deriving_yojson
  lwt_ppx
  argon2
  js_of_ocaml
  js_of_ocaml-lwt 
  js_of_ocaml-ppx 
  yojson 
  brr
  js_of_ocaml-ppx
  mirage-crypto 
  mirage-crypto-rng 
  cstruct
  ptime
 ))
