module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type

let lock_account (module Db : DB) email =
  let query =
    let open Caqti_request.Infix in
    (T.string ->. T.unit)
      "UPDATE users SET locked_until = NOW() + INTERVAL '1 hour' WHERE email = ?"
  in
  Db.exec query email

let is_account_locked (module Db : DB) email =
  let open Caqti_request.Infix in
  let query =
    (T.string ->? T.bool)
      "SELECT locked_until > NOW() FROM users WHERE email = ?"
  in
  match%lwt Db.find_opt query email with
  | Ok (Some locked) -> Lwt.return locked
  | Ok None -> Lwt.return false
  | Error _ -> Lwt.return false

let increment_failed_attempts (module Db : DB) email =
  let query =
    let open Caqti_request.Infix in
    (T.string ->. T.unit)
      "UPDATE users SET failed_attempts = failed_attempts + 1 WHERE email = ?"
  in
  Db.exec query email

let reset_failed_attempts (module Db : DB) email =
  let query =
    let open Caqti_request.Infix in
    (T.string ->. T.unit)
      "UPDATE users SET failed_attempts = 0 WHERE email = ?"
  in
  Db.exec query email

let get_failed_attempts (module Db : DB) email =
  let open Caqti_request.Infix in
  let query =
    (T.string ->? T.int)
      "SELECT failed_attempts FROM users WHERE email = ?"
  in
  match%lwt Db.find_opt query email with
  | Ok (Some count) -> Lwt.return count
  | Ok None -> Lwt.return 0
  | Error _ -> Lwt.return 0