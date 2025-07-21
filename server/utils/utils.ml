open Scrypt

module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type

let () = Dotenv.export () 

let get_hash_secret =
  match Sys.getenv_opt "SECRET" with
  | Some url -> url
  | None -> failwith "SECRET not set"

let add_user (module Db : Caqti_lwt.CONNECTION) name email password =
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


let hash_password password:string =
  encrypt_exn password get_hash_secret

let verify_password hash_password =
  decrypt_exn hash_password get_hash_secret 
