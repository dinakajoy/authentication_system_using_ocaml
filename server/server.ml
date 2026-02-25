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

type forgot_password_request = {
  email : string;
} [@@deriving yojson]

type reset_password_request = {
  token : string;
  password : string;
  confirm_password : string;
} [@@deriving yojson]

let send_mail name email subject content = 
  let open Lwt.Infix in
  Utils.Helpers.send_email
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
    <p>You can also copy and paste the following link in your browwser:</p>
    http://localhost:8080/verify_email?token=%s
    <br />
    <p>If you didn’t create this account, ignore this email.</p>
  |} name token token in
  send_mail name email "Verification Email" html

let send_password_reset_email name email token =
  let html = Printf.sprintf {|
    <h1>Hi %s!</h1>
    <p>Please click the link below to change your password.</p>
    <p>This link expires in 1 hour.</p>
    <a href="http://localhost:8080/reset-password?token=%s">
      Change Password
    </a>
    <p>You can also copy and paste the following link in your browwser:</p>
    http://localhost:8080/reset-password?token=%s
    <br />
    <p>If you didn’t request to change your password, ignore this email.</p>
  |} name token token in
  send_mail name email "Password Reset Email" html

let login_handler login_data request =
  match Yojson.Safe.from_string login_data with
  | exception _ ->
    Dream.json ~status:`Bad_Request {|{ "error": "Invalid input" }|}
  | json -> (
    match login_request_of_yojson json with
    | Error e ->
      Dream.json ~status:`Bad_Request
        (Printf.sprintf {|{ "error": "Invalid input: %s" }|} e)
    | Ok { email; password } -> 
      let open Lwt.Syntax in
      Dream.sql request (fun db ->
        let ip = Dream.client request in
        let* limited = Utils.Rate_limiter.is_rate_limited db email ip in
        if limited then
          Dream.json ~status:`Too_Many_Requests
            {|{ "error": "Too many failed login attempts. Please try again later." }|}
        else 
          let* account_locked = Utils.Lockout.is_account_locked db email in
          if account_locked then
            Dream.json ~status:`Forbidden
              {|{ "error": "Account is temporarily locked due to multiple failed login attempts. Please try again later." }|}
          else
            let* user_result = Utils.User.get_user db email in
            match user_result with
            | Error e ->
              Dream.log "DB error during login: %s" (Caqti_error.show e);
              Dream.json ~status:`Internal_Server_Error
                {|{ "error": "Server error" }|}
            | Ok None ->
              (* Record the failed login attempt *)
              let* _ = Utils.Rate_limiter.record_login_attempt db email ip false in
              Dream.json ~status:`Unauthorized {|{ "error": "Invalid email or password" }|}
            | Ok (Some (user_id, _email, hashed_password, is_verified)) ->
                if not is_verified then
                  Dream.json ~status:`Forbidden
                    {|{ "error": "If you are registered, please check your email for next instructions." }|}
                else
                  match Utils.Helpers.verify_password hashed_password password with
                  | Ok _ ->
                    let* () = Dream.invalidate_session request in
                    let* () = Dream.set_session_field request "user_id" (string_of_int user_id) in
                      (* Record successful login *)
                      let* _ = Utils.Rate_limiter.record_login_attempt db email ip true in
                      Dream.json ~status:`OK
                        {|{ "status": "ok", "message": "Login was successful" }|}
                  | Error _ -> 
                    (* Record the failed login attempt *)
                    let* _ = Utils.Rate_limiter.record_login_attempt db email ip false in
                    let* _ = Utils.Lockout.increment_failed_attempts db email in
                    let* count_failed_attempts = Utils.Lockout.get_failed_attempts db email in
                    if count_failed_attempts >= 5 then
                      let* _ = Utils.Lockout.lock_account db email in
                        Dream.json ~status:`Forbidden
                          {|{ "error": "Too many failed login attempts. Account is locked." }|}
                    else
                      Dream.json ~status:`Unauthorized
                        {|{ "error": "Invalid email or password" }|} 
      )
  )

let registeration_handler user_details request =
  match Yojson.Safe.from_string user_details with
  | exception _ ->
    Dream.json ~status:`Bad_Request {|{ "error": "Invalid input" }|}
  | json -> (
    match registration_request_of_yojson json with
    | Error e ->
      Dream.json ~status:`Bad_Request
        (Printf.sprintf {|{ "error": "Invalid input: %s" }|} e)
    | Ok { name; email; password } -> 
      let open Lwt.Syntax in
      Dream.sql request (fun db ->
        let* result = Utils.User.get_user db email in
        match result with
        | Error e ->
          Dream.json ~status:`Internal_Server_Error
            (Printf.sprintf {|{ "error": "DB error: %s" }|} (Caqti_error.show e))
        | Ok (Some _) ->
          Dream.json ~status:`Unauthorized {|{ "error": "Email already exists" }|}
        | Ok None ->
          let hashed_password =
            match Utils.Helpers.hash_password password with
            | Error err ->
              failwith ("Hashing failed: " ^ Argon2.ErrorCodes.message err)
            | Ok (_hash, encoded) -> encoded
            in
            let* result = Utils.Verification.add_user_and_verification_token db name email hashed_password in
            match result with
            | Ok (raw_token) ->
              (* Send email *)
              let* send_mail_result =
                send_account_verification_email name email raw_token in
                (match send_mail_result with
                | true ->
                  Dream.json
                    {|{ "status": "ok", "message": "User has been registered. Please check your email for account verification instructions." }|}
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
    let token_hash = Utils.Helpers.hash_token token in
    let* verification_data = Utils.Verification.get_verification_token db token_hash in
    match verification_data with
    | Error e ->
      Dream.log "DB error generating new token: %s" (Caqti_error.show e);
      Dream.html "<h2 style=\"color: red;height:100vh;display:flex;align-items:center;justify-content:center;\">Error verifying account.</h2>"
    | Ok None ->
        Dream.html "<h2 style=\"color: red;height:100vh;display:flex;align-items:center;justify-content:center;\">Invalid verification link</h2>"
    | Ok (Some (user_id, token_hash, expires_at, reason)) ->
        if (Utils.Helpers.is_token_valid expires_at && reason = "verification") then
           (* Mark user as verified *)
          let* update_user = Utils.User.update_user_as_verified db user_id in
          match update_user with
          | Ok () ->
              (* Delete the used token *)
              let* _ = Utils.Verification.delete_verification_token db token_hash in
              Dream.redirect request "/login"
          | Error e ->
            Dream.log "DB error updating user verification status: %s" (Caqti_error.show e);
            Dream.html "<h2 style=\"color: red;height:100vh;display:flex;align-items:center;justify-content:center;\">Failed to verify email. Please try again.</h2>"
        else (
         (* Delete the expired token *)
          let* _ = Utils.Verification.delete_verification_token db token_hash in
          (* Generate token and expiration *)
          let new_raw_token = Utils.Helpers.generate_token () in
          let new_token_hash = Utils.Helpers.hash_token new_raw_token in
          let new_expiry = Utils.Helpers.token_expiry_time () in
          (* Insert verification token linked to the user *)
          let* new_user_verification_token =
            Utils.Verification.add_verification_token db user_id new_token_hash new_expiry "verification"
          in
          match new_user_verification_token with
          | Ok () -> 
              (* Fetch user email and name for sending email *)
              let* user_result = Utils.User.get_user_by_id db user_id in
              (match user_result with
              | Error e ->
                Dream.log "DB error fetching user details: %s" (Caqti_error.show e);
                Dream.html "<h2 style=\"color: red;height:100vh;display:flex;align-items:center;justify-content:center;\">Failed to fetch user details, try again.</h2>"
              | Ok None ->
                Dream.html "<h2 style=\"color: red;height:100vh;display:flex;align-items:center;justify-content:center;\">Invalid account</h2>"
              | Ok (Some (name, email, _is_verified, _created_at)) ->
                  (* Send new verification email *)
                  let* send_mail_result =
                    send_account_verification_email name email new_raw_token
                  in
                  (match send_mail_result with
                  | true ->
                    Dream.html "<h2 style=\"color: red;height:100vh;display:flex;align-items:center;justify-content:center;\">Link has expired. Check your email for new verification details.</h2>"
                  | false ->
                    Dream.html "<h2 style=\"color: red;height:100vh;display:flex;align-items:center;justify-content:center;\">Failed to send verification mail.</h2>"))
          | Error e ->
            Dream.log "DB error generating new token: %s" (Caqti_error.show e);
            Dream.html "<h2 style=\"color: red;height:100vh;display:flex;align-items:center;justify-content:center;\">Failed to generate new token.</h2>"
        )
  )

let forgot_password_handler forgot_password_data request =
  match Yojson.Safe.from_string forgot_password_data with
  | exception _ ->
    Dream.json ~status:`Bad_Request {|{ "error": "Invalid input" }|}
  | json -> (
    match forgot_password_request_of_yojson json with
    | Error e ->
      Dream.json ~status:`Bad_Request
        (Printf.sprintf {|{ "error": "Invalid input: %s" }|} e)
    | Ok { email } -> 
      let open Lwt.Syntax in
      Dream.sql request (fun db ->
        let* result = Utils.User.get_user db email in
        match result with
        | Error e ->
          Dream.json ~status:`Internal_Server_Error
            (Printf.sprintf {|{ "error": "DB error: %s" }|} (Caqti_error.show e))
        | Ok None ->
          (* Send this so users don't know if email exists or not *)
          Dream.json ~status:`OK {|{ "status": "ok", "message": "Reset password details has been sent." }|}
        | Ok (Some (id, email, _hashed_password, is_verified)) ->
            if not is_verified then
              Dream.json ~status:`Unauthorized
                {|{ "error": "If you are registered, please check your email for next instructions." }|}
            else
              (* Generate a password reset token *)
              let raw_token = Utils.Helpers.generate_token () in
              let token_hash = Utils.Helpers.hash_token raw_token in
              let expiry = Utils.Helpers.token_expiry_time () in
              let* add_token_result = Utils.Verification.add_verification_token db id token_hash expiry "password_reset" in
              match add_token_result with
              | Ok () ->
                  (* Send password reset email *)
                  let* send_mail_result =
                    send_password_reset_email "User" email raw_token in
                    (match send_mail_result with
                    | true ->
                      Dream.json
                        {|{ "status": "ok", "message": "Details to reset your password has been sent to your email." }|}
                    | false ->
                      Dream.json ~status:`Internal_Server_Error
                        {|{ "error": "Failed to send reset password mail" }|})
              | Error e ->
                  Dream.json ~status:`Internal_Server_Error
                    (Printf.sprintf {|{ "error": "Failed to generate reset token: %s" }|} (Caqti_error.show e))
      )
    )

let reset_password_page token request =
  let open Lwt.Syntax in
  Dream.sql request (fun db ->
    let token_hash = Utils.Helpers.hash_token token in
    let* reset_data = Utils.Verification.get_verification_token db token_hash in
    match reset_data with
    | Error e ->
      Dream.log "DB error verifying token: %s" (Caqti_error.show e);
      Dream.html "<h2 style=\"color: red;height:100vh;display:flex;align-items:center;justify-content:center;\">Error verifying reset link.</h2>"
    | Ok None ->
        Dream.html "<h2 style=\"color: red;height:100vh;display:flex;align-items:center;justify-content:center;\">Invalid reset password link</h2>"
    | Ok (Some (_user_id, _token_hash, expires_at, reason)) ->
        if (Utils.Helpers.is_token_valid expires_at && reason = "password_reset") then
          Dream.html (Pages.reset_password_page ())
        else
          let* _ = Utils.Verification.delete_verification_token db token_hash in
          Dream.html "<h2 style=\"color: red;height:100vh;display:flex;align-items:center;justify-content:center;\">Link has expired or is invalid. Please request a new password reset link as this will be deleted now.</h2>"
  )

let reset_password_handler new_password_data request =
  match Yojson.Safe.from_string new_password_data with
  | exception _ ->
    Dream.json ~status:`Bad_Request {|{ "error": "Invalid input" }|}
  | json -> (
    match reset_password_request_of_yojson json with
    | Error e ->
      Dream.json ~status:`Bad_Request
        (Printf.sprintf {|{ "error": "Invalid input: %s" }|} e)
    | Ok { token; password; confirm_password } -> 
      let open Lwt.Syntax in
      Dream.sql request (fun db ->
        let token_hash = Utils.Helpers.hash_token token in
        let* result = Utils.Verification.get_verification_token db token_hash in
        match result with
        | Error e ->
          Dream.json ~status:`Internal_Server_Error
            (Printf.sprintf {|{ "error": "DB error: %s" }|} (Caqti_error.show e))
        | Ok None ->
          Dream.json ~status:`Unauthorized {|{ "error": "Invalid account" }|}
        | Ok (Some (user_id, token_hash, expires_at, reason)) ->
          if (not (Utils.Helpers.is_token_valid expires_at) && reason <> "password_reset") then
            Dream.json ~status:`Bad_Request {|{ "error": "Link has expired or is invalid. Please request a new password reset." }|}
          else if confirm_password = "" || password = "" then
            Dream.json ~status:`Bad_Request {|{ "error": "Please fill all fields" }|}
          else if confirm_password = password then
            let hashed_password =
              match Utils.Helpers.hash_password password with
              | Error err ->
                failwith ("Hashing failed: " ^ Argon2.ErrorCodes.message err)
              | Ok (_hash, encoded) -> encoded
              in
              let* update_password = Utils.User.update_user_password db user_id hashed_password in
                match update_password  with
                | Ok () ->  
                  let* () = Dream.invalidate_session request in
                  (* Delete the used token *)
                  let* _ = Utils.Verification.delete_verification_token db token_hash in
                    Dream.json
                      (Printf.sprintf {|{ "status": "ok", "message": "Password updated successfully. You can now login with your new password." }|})
                | Error _ -> Dream.json ~status:`Unauthorized
                {|{ "error": "Invalid request" }|}
          else 
            Dream.json ~status:`Bad_Request
              {|{ "error": "Password and confirm password do not match" }|}
      )
    )

let dashboard_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some user_id ->
    Dream.sql request (fun db ->
      let user_id = int_of_string user_id in
      let* user = Utils.User.get_user_by_id db user_id in
      match user with
      | Error e ->
          Dream.json ~status:`Internal_Server_Error
            (Printf.sprintf {|{ "error": "DB error: %s" }|} (Caqti_error.show e))
      | Ok None -> Dream.redirect request "/login"
      | Ok (Some user) -> Dream.html (Pages.dashboard_page user))

let require_auth handler request =
  match Dream.session_field request "user_id" with
  | None ->
      Dream.redirect request "/login"
  | Some _ ->
      handler request

let check_db_connection () =
  let open Lwt.Syntax in
  let db_uri = Uri.of_string (Utils.Helpers.get_env "DATABASE_URL") in
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
  let db_url = Utils.Helpers.get_env "DATABASE_URL" in
  Dream.serve 
  @@ Dream.logger
  @@ Dream.sql_pool db_url
  @@ Dream.sql_sessions
  @@ Dream.router [
    Dream.get "/" (require_auth dashboard_handler);
    Dream.get "/dashboard" (require_auth dashboard_handler);
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
          Dream.html "<h1 style=\"color: red;height:100vh;display:flex;align-items:center;justify-content:center;\">Invalid verification link</h1>"
    );
    Dream.get "/forgot-password" (fun _ -> Dream.html (Pages.forgot_password_page ()));
    Dream.post "/forgot-password" (fun request -> 
      let* body = Dream.body request in
      forgot_password_handler body request
    );
    Dream.get "/reset-password" (fun request ->
      match Dream.query request "token" with
      | Some token ->
          reset_password_page token request
      | None ->
          Dream.html "<h2 style=\"color: red;height:100vh;display:flex;align-items:center;justify-content:center;\">Invalid reset password link</h2>"
    );
    Dream.post "/reset-password" (fun request -> 
      let* body = Dream.body request in
      reset_password_handler body request
    );
    Dream.get "/logout" (fun request -> 
      let* () = Dream.invalidate_session request in
      Dream.redirect request "/login"
    ); 
    Dream.get "/static/**" (Dream.static "client")
  ]

let () = Lwt_main.run (start_server ())
