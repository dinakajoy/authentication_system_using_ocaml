open Argon2

module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type

let () = Dotenv.export () 

let get_hash_secret =
  match Sys.getenv_opt "SECRET" with
  | Some url -> url
  | None -> failwith "SECRET not set"

let add_user (module Db : DB) name email password =
  let query =
    let open Caqti_request.Infix in
    (T.(t3 T.string T.string T.string) ->. T.unit) 
    "INSERT INTO users (name, email, password) VALUES (?, ?, ?)"
  in
  Db.exec query (name, email, password)

let get_user (module Db : DB) email =
  let open Caqti_request.Infix in
  let query = (T.string ->? T.(t2 T.string T.string))
    "SELECT email, password FROM users WHERE email = ?" in
  let%lwt result = Db.find_opt query email in
  Lwt.return result

let hash_password password =
  let encoded_len = Argon2.encoded_len ~t_cost:2 ~m_cost:65536 ~parallelism:1 ~salt_len:(String.length "0000000000000000") ~hash_len:32 ~kind:D in
  Argon2.hash ~t_cost:2 ~m_cost:65536 ~parallelism:1 ~pwd:password ~salt:"0000000000000000" ~kind:D ~hash_len:32 ~encoded_len
      ~version:VERSION_NUMBER

let verify_password hash password =
  Argon2.verify ~encoded:hash ~kind:D ~pwd:password
