module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type

let add_verification_token (module Db : DB) user_id token_hash expiry reason =
  let query =
    let open Caqti_request.Infix in
    (T.(t4 int string ptime string) ->. T.unit)
      "INSERT INTO email_verification_tokens (user_id, token_hash, expires_at, reason) VALUES (?, ?, ?, ?)"
  in
  Db.exec query (user_id, token_hash, expiry, reason)

let get_verification_token (module Db : DB) token_hash =
  let open Caqti_request.Infix in
  let query = (T.string ->? T.(t4 int string ptime string))
    "SELECT user_id, token_hash, expires_at, reason FROM email_verification_tokens WHERE token_hash = ?" in
  Db.find_opt query token_hash

let delete_verification_token (module Db : DB) token_hash =
  let query =
    let open Caqti_request.Infix in
    (T.string ->. T.unit)
      "DELETE FROM email_verification_tokens WHERE token_hash = ?"
  in
  Db.exec query token_hash

let add_user_and_verification_token (module Db : DB) name email hashed_password =
  let open Lwt_result.Syntax in
  Db.with_transaction (fun _ ->
    let* user_id =
      User.add_user (module Db : DB) name email hashed_password
    in

    let raw_token = Helpers.generate_token () in
    let token_hash = Helpers.hash_token raw_token in
    let expiry = Helpers.token_expiry_time () in

    let* () =
      add_verification_token (module Db : DB) user_id token_hash expiry "verification"
    in

    Lwt_result.return raw_token
  )
