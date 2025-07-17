open Scrypt
(* module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request *)
module type DB = Caqti_lwt.CONNECTION
module T = Caqti_type

(* let add_user =
  let query =
    let open Caqti_request.Infix in
    (T.string ->. T.unit)
    "INSERT INTO user (text) VALUES ($1)" in
  fun text (module Db : DB) ->
    let%lwt unit_or_error = Db.exec query text in
    Caqti_lwt.or_fail unit_or_error *)

let get_user (module Db : DB) email =
  let open Caqti_request.Infix in
  let query = (T.string ->? T.(t2 T.string T.string))
    "SELECT email, password FROM users WHERE email = ?" in
  let%lwt result = Db.find_opt query email in
  Lwt.return result


let hash_password password:string =
  encrypt_exn "my secret data" password

let verify_password password hash_password =
  decrypt_exn hash_password password
