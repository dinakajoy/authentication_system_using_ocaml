open Argon2
open Lwt.Infix

module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type

let get_env name =
  match Sys.getenv_opt name with
  | Some v -> v
  | None ->
      failwith ("Missing required environment variable: " ^ name)

let generate_token () =
  Mirage_crypto_rng.generate 32
  |> Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet

let hash_token token =
  Digestif.SHA256.(to_hex (digest_string token))

let token_expiry_time () =
  let now = Ptime_clock.now () in
  let one_hour = Ptime.Span.of_int_s 3600 in
  match Ptime.add_span now one_hour with
  | Some t -> t
  | None -> failwith "Failed to calculate expiry time"

let is_token_valid expires_at =
  let now = Ptime_clock.now () in
  Ptime.compare now expires_at <= 0

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
      add_user (module Db : DB) name email hashed_password
    in

    let raw_token = generate_token () in
    let token_hash = hash_token raw_token in
    let expiry = token_expiry_time () in

    let* () =
      add_verification_token (module Db : DB) user_id token_hash expiry "verification"
    in

    Lwt_result.return raw_token
  )

let hash_password password =
  let encoded_len = Argon2.encoded_len ~t_cost:2 ~m_cost:65536 ~parallelism:1 ~salt_len:(String.length "0000000000000000") ~hash_len:32 ~kind:D in
  Argon2.hash ~t_cost:2 ~m_cost:65536 ~parallelism:1 ~pwd:password ~salt:"0000000000000000" ~kind:D ~hash_len:32 ~encoded_len
      ~version:VERSION_NUMBER

let verify_password hash password =
  Argon2.verify ~encoded:hash ~kind:D ~pwd:password

let send_email ~to_email ~to_name ~subject ~html_content =
  let uri = Uri.of_string "https://api.brevo.com/v3/smtp/email" in
  let api_key = get_env "BREVO_KEY" in

  let body =
  `Assoc [
    "sender", `Assoc [
        ("name", `String "OCaml Auth App");
        ("email", `String "dinakajoy@gmail.com")
    ];
    "to",
      `List [
        `Assoc [
          ("email", `String to_email);
          ("name", `String to_name)
        ]
      ];
    "subject", `String subject;
    "htmlContent", `String html_content;
  ]
  |> Yojson.Safe.to_string
  in
  let headers =
    Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "api-key" api_key
    |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
  in

  Cohttp_lwt_unix.Client.post
    ~headers
    ~body:(Cohttp_lwt.Body.of_string body)
    uri
  >>= fun (resp, body_stream) ->
  let status = Cohttp.Response.status resp in
  Cohttp_lwt.Body.to_string body_stream >>= fun response_body ->

  if Cohttp.Code.(code_of_status status = 201) then
    Lwt.return_ok ()
  else
    Lwt.return_error (status, response_body)

let record_login_attempt (module Db : DB) email ip success = 
  let query =
    let open Caqti_request.Infix in
    (T.(t3 string string bool) ->. T.unit)
      "INSERT INTO login_attempts (email, ip_address, success) VALUES (?, ?, ?)"
  in
  Db.exec query (email, ip, success)

let count_recent_failures (module Db : DB) email ip =
  let open Caqti_request.Infix in
  let query = (T.(t2 string string) ->? T.int)
    "SELECT COUNT(*) FROM login_attempts WHERE email = ? AND ip_address = ? AND success = false AND attempted_at > NOW() - INTERVAL '10 minutes'" in
  Db.find_opt query (email, ip)

let is_rate_limited db email ip =
  count_recent_failures db email ip >>= fun result ->
  match result with
  | Ok (Some count) -> Lwt.return (count >= 5)
  | Ok None -> Lwt.return false
  | Error _ -> Lwt.return false

let lock_account (module Db : DB) email =
  let query =
    let open Caqti_request.Infix in
    (T.string ->. T.unit)
      "UPDATE users SET locked_until = NOW() + INTERVAL '1 hour' WHERE email = ?"
  in
  Db.exec query email

let is_account_locked (module Db : DB) email =
  let open Caqti_request.Infix in
  let query = (T.string ->? T.bool)
    "SELECT locked_until > NOW() FROM users WHERE email = ?" in
  Db.find_opt query email
