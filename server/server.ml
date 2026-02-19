open Lwt.Syntax

module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type

type login_request = {
  email : string;
  password : string;
} [@@deriving yojson]

type registration_request = {
  name : string;
  email : string;
  password : string;
} [@@deriving yojson]

let send_mail name email subject content = 
  let open Lwt.Infix in
  Utils.send_email
    ~to_email: email
    ~to_name: name
    ~subject: subject
    ~html_content:content
  >>= function
  | Ok () -> Lwt.return_true
  | Error (_status, err) ->
      Dream.log "Email error: %s" err;
      Lwt.return_false

let send_account_verification_email name email token =
  let html = Printf.sprintf {|
    <h1>Hi %s!</h1>
    <p>Please click the link below to verify your email.</p>
    <p>This link expires in 1 hour.</p>
    <a href="http://localhost:8080/verify_email?token=%s">
      Verify Email
    </a>
    <p>You can also copy and paste the following token in the verification page:</p>
    http://localhost:8080/verify_email?token=%s
    <br />
    <p>If you didn’t create this account, ignore this email.</p>
  |} name token token in
  send_mail name email "Verification Email" html

let login_handler login_data request =
  match Yojson.Safe.from_string login_data with
  | exception _ ->
    Dream.json ~status:`Bad_Request {|{ "error": "Invalid JSON" }|}
  | json -> (
    match login_request_of_yojson json with
    | Error e ->
      Dream.json ~status:`Bad_Request
        (Printf.sprintf {|{ "error": "Invalid input: %s" }|} e)
    | Ok { email; password } -> 
      let open Lwt.Syntax in
      Dream.sql request (fun db ->
        let* result = Utils.get_user db email in
        match result with
        | Error e ->
          Dream.json ~status:`Internal_Server_Error
            (Printf.sprintf {|{ "error": "DB error: %s" }|} (Caqti_error.show e))
        | Ok None ->
          Dream.json ~status:`Unauthorized {|{ "error": "User not found" }|}
        | Ok (Some (_email, hashed_password, is_verified)) ->
            if not is_verified then
              Dream.json ~status:`Unauthorized
                {|{ "error": "Account not verified. Please check your email for verification instructions." }|}
            else
              match Utils.verify_password hashed_password password with
              | Ok _ ->  Dream.json
                (Printf.sprintf {|{ "status": "ok", "message": "Welcome %s" }|} email)
              | Error _ -> Dream.json ~status:`Unauthorized
                {|{ "error": "Invalid email or password" }|} 
      )
    )

let registeration_handler user_details request =
  match Yojson.Safe.from_string user_details with
  | exception _ ->
    Dream.json ~status:`Bad_Request {|{ "error": "Invalid JSON" }|}
  | json -> (
    match registration_request_of_yojson json with
    | Error e ->
      Dream.json ~status:`Bad_Request
        (Printf.sprintf {|{ "error": "Invalid input: %s" }|} e)
    | Ok { name; email; password } -> 
      let open Lwt.Syntax in
      Dream.sql request (fun db ->
        let* result = Utils.get_user db email in
        match result with
        | Error e ->
          Dream.json ~status:`Internal_Server_Error
            (Printf.sprintf {|{ "error": "DB error: %s" }|} (Caqti_error.show e))
        | Ok (Some _) ->
          Dream.json ~status:`Unauthorized {|{ "error": "Email already exists" }|}
        | Ok None ->
          let hashed_password =
            match Utils.hash_password password with
            | Error err ->
              failwith ("Hashing failed: " ^ Argon2.ErrorCodes.message err)
            | Ok (_hash, encoded) -> encoded
            in
            let* result = Utils.add_user_and_verification_token db name email hashed_password in
            match result with
            | Ok (raw_token) ->
              (* Send email *)
              let* send_mail_result =
                send_account_verification_email name email raw_token in
                (match send_mail_result with
                | true ->
                  Dream.json
                    {|{ "status": "ok", "message": "User has been registered. Please check your email for verification instructions." }|}
                | false ->
                  Dream.json ~status:`Internal_Server_Error
                    {|{ "error": "Failed to send verification mail" }|})
            | Error e ->
              Dream.json ~status:`Internal_Server_Error
                (Printf.sprintf {|{ "error": "Failed to register user: %s" }|} (Caqti_error.show e)))
      )

let verify_account_handler token request =
  let open Lwt.Syntax in
  Dream.sql request (fun db ->
    let token_hash = Utils.hash_token token in
    let* verification_data = Utils.get_verification_token db token_hash in
    match verification_data with
    | Error e ->
      Dream.json ~status:`Internal_Server_Error
        (Printf.sprintf {|{ "error": "DB error: %s" }|} (Caqti_error.show e))
    | Ok None ->
        Dream.json ~status:`Bad_Request {|{ "error": "Invalid token." }|}
    | Ok (Some (user_id, token_hash, expires_at)) ->
        if not (Utils.is_token_valid expires_at) then
          (* Delete the expired token *)
          let* _ = Utils.delete_verification_token db token_hash in
          (* Generate token and expiration *)
          let new_raw_token = Utils.generate_token () in
          let new_token_hash = Utils.hash_token new_raw_token in
          let new_expiry = Utils.token_expiry_time () in
          (* Insert verification token linked to the user *)
          let* new_user_verification_token =
            Utils.add_verification_token db user_id new_token_hash new_expiry
          in
          match new_user_verification_token with
          | Ok () -> 
              (* Fetch user email and name for sending email *)
              let* user_result = Utils.get_user_by_id db user_id in
              (match user_result with
              | Error e ->
                Dream.json ~status:`Internal_Server_Error
                  (Printf.sprintf {|{ "error": "Failed to fetch user details: %s" }|} (Caqti_error.show e))
              | Ok None ->
                Dream.json ~status:`Unauthorized {|{ "error": "User not found" }|}
              | Ok (Some (name, email)) ->
                  (* Send new verification email *)
                  let* send_mail_result =
                    send_account_verification_email name email new_raw_token
                  in
                  (match send_mail_result with
                  | true ->
                    Dream.json ~status:`Bad_Request {|{ "error": "Link has expired. Check your email for new verification details." }|}
                  | false ->
                    Dream.json ~status:`Internal_Server_Error
                      {|{ "error": "Failed to send verification mail" }|}))
          | Error e ->
              Dream.json ~status:`Internal_Server_Error
                (Printf.sprintf {|{ "error": "Failed to generate new token: %s" }|} (Caqti_error.show e))
        else (
          (* Mark user as verified *)
          let* update_user = Utils.update_user_as_verified db user_id in
          match update_user with
          | Ok () ->
              (* Delete the used token *)
              let* _ = Utils.delete_verification_token db token_hash in
              Dream.html (Pages.login_page ())
          | Error e ->
              Dream.json ~status:`Internal_Server_Error
                (Printf.sprintf {|{ "error": "Failed to verify email: %s" }|} (Caqti_error.show e))
        )
  )

let check_db_connection () =
  let open Lwt.Syntax in
  let db_uri = Uri.of_string (Utils.get_env "DATABASE_URL") in
  match%lwt Caqti_lwt_unix.connect db_uri with
  | Ok (module Db : Caqti_lwt.CONNECTION) ->
      let query =
        Caqti_request.Infix.(
          Caqti_type.unit ->! Caqti_type.string
        )
        "SELECT 'Connected to DB'::text"
      in
      let* result = Db.find query () in
      (match result with
       | Ok msg ->
           Dream.log "✅ DB Connection OK: %s" msg;
           Lwt.return_unit
       | Error err ->
           Dream.log "❌ Query failed: %s" (Caqti_error.show err);
           Lwt.fail_with "DB query failed")
  | Error err ->
      Dream.log "❌ Failed to connect to DB: %s" (Caqti_error.show err);
      Lwt.fail_with "DB connection failed"

let start_server () =
  Mirage_crypto_rng_unix.use_default ();
  let* () = check_db_connection () in
  let db_url = Utils.get_env "DATABASE_URL" in
  Dream.serve 
  @@ Dream.logger
  @@ Dream.sql_pool db_url
  @@ Dream.router [
    Dream.get "/" (fun _ -> Dream.html (Pages.login_page ()));
    Dream.get "/login" (fun _ -> Dream.html (Pages.login_page ()));
    Dream.post "/login" (fun request -> 
      let* body = Dream.body request in
      login_handler body request
    );
    Dream.get "/register" (fun _ -> Dream.html (Pages.registration_page ()));
    Dream.post "/register" (fun request -> 
      let* body = Dream.body request in
      registeration_handler body request
    );
    Dream.get "/verify_email" (fun request ->
      match Dream.query request "token" with
      | Some token ->
          verify_account_handler token request
      | None ->
          Dream.html "Invalid verification link"
    );
    Dream.get "/static/**" (Dream.static "client")
  ]

let () = Lwt_main.run (start_server ())
