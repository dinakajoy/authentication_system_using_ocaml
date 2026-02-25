module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type

let add_user (module Db : DB) name email password =
  let query =
    let open Caqti_request.Infix in
    (T.(t3 string string string) ->! T.int)
    "INSERT INTO users (name, email, password) VALUES (?, ?, ?) RETURNING id"
  in
  Db.find query (name, email, password)

let get_user (module Db : DB) email =
  let open Caqti_request.Infix in
  let query = (T.string ->? T.(t4 int string string bool))
    "SELECT id, email, password, is_verified FROM users WHERE email = ?" in
  Db.find_opt query email

let get_user_by_id (module Db : DB) user_id =
  let open Caqti_request.Infix in
  let query = (T.int ->? T.(t4 string string bool string))
    "SELECT name, email, is_verified, created_at FROM users WHERE id = ?" in
  Db.find_opt query user_id

let update_user_as_verified (module Db : DB) user_id =
  let query =
    let open Caqti_request.Infix in
    (T.int ->. T.unit)
      "UPDATE users SET is_verified = TRUE WHERE id = ?"
  in
  Db.exec query user_id

let update_user_password (module Db : DB) user_id password =
  let query =
    let open Caqti_request.Infix in
    (T.(t2 string int) ->. T.unit)
      "UPDATE users SET password = ? WHERE id = ?"
  in
  Db.exec query (password, user_id)
