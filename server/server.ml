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

let send_mail username email subject content = 
  let open Lwt.Infix in
  Utils.send_email
    ~to_email: email
    ~to_name: username
    ~subject: subject
    ~html_content:content
  >>= function
  | Ok () -> Lwt.return_true
  | Error (_status, err) ->
      Dream.log "Email error: %s" err;
      Lwt.return_false

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
                {|{ "error": "Email not verified. Please check your inbox." }|}
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
        | Ok None ->
          let hashed_password =
            match Utils.hash_password password with
            | Ok (_hash, encoded) -> encoded
            | Error err ->
              failwith ("Hashing failed: " ^ Argon2.ErrorCodes.message err) in
          
              (let%lwt result =
                Db.with_transaction db (fun db ->
                  let%lwt user_id =
                    Utils.add_user db name email hashed_password 
                  in

                  (* Generate token *)
                  let raw_token = Utils.generate_token () in
                  let token_hash = Utils.hash_token raw_token in

                  (* Compute expiry which is current time + 1hour *)
                  let expiry = Utils.token_expiry_time () in

                  (* Insert verification token linked to the user *)
                  let%lwt () =
                    Utils.add_verification_token db user_id token_hash expiry
                  in
                  Lwt.return (Ok (user_id, raw_token))
                )
              in
              (* Handle transaction result *)
              match result with
              | Ok (_user_id, raw_token) ->
                  (* Build the verification email *)
                  let html =
                    Printf.sprintf {|
                      <h1>Hi!</h1>
                      <p>Please click the link below to verify your email.</p>
                      <p>This link expires in 1 hour.</p>
                      <a href="http://localhost:8080/verify_email?token=%s">
                        Verify Email
                      </a>
                      <p>If you didn’t create this account, ignore this email.</p>
                    |} raw_token
                  in

                  (* Send email *)
                  let%lwt send_result =
                    send_mail name email "Verification Email" html
                  in

                  (match send_result with
                  | true ->
                      Dream.json
                        {|{ "status": "ok", "message": "User has been registered. Please check your email for verification instructions." }|}
                  | false ->
                      Dream.json ~status:`Internal_Server_Error
                        {|{ "error": "Failed to send verification mail" }|})

              | Error e ->
                  Dream.json ~status:`Internal_Server_Error
                    (Printf.sprintf {|{ "error": "Failed to register user: %s" }|} (Caqti_error.show e)))

        | Ok (Some _) ->
          Dream.json ~status:`Unauthorized {|{ "error": "Email already exists" }|}
      )
    )

let verify_email_handler token request =
  let open Lwt.Syntax in
  Dream.sql request (fun db ->
    let token_hash = Utils.hash_token token in
    let%lwt verification_data = Utils.get_verification_token db token_hash in
    match verification_data with
    | Error e ->
        Dream.json ~status:`Internal_Server_Error
          (Printf.sprintf {|{ "error": "DB error: %s" }|} (Caqti_error.show e))
    | Ok None ->
        Dream.json ~status:`Bad_Request {|{ "error": "Invalid or expired token" }|}
    | Ok (Some (user_id, expires_at)) ->
        let expires_at =
          match Ptime.of_rfc3339 expires_at with
          | Ok (t, _, _) -> t
          | Error _ -> failwith "Invalid expiry time format in DB"
        in
        if not (Utils.is_token_valid expires_at) then
          Dream.json ~status:`Bad_Request {|{ "error": "Token has expired" }|}
        else (
          (* Mark user as verified *)
          let%lwt update_user = Utils.update_user db user_id in
          match update_user with
          | Ok () ->
              (* Delete the used token *)
              let%lwt _ = Utils.delete_verification_token db token_hash in
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
  let db_url = Utils.get_env "DATABASE_URL" in
  let* () = check_db_connection () in
  Dream.serve 
  @@ Dream.logger
  @@ Dream.sql_pool db_url
  @@ Dream.router [
    Dream.get "/" (fun _ -> Dream.html (Pages.login_page ()));
    Dream.get "/login" (fun _ -> Dream.html (Pages.login_page ()));
    Dream.post "/login" (fun request -> 
      let%lwt body = Dream.body request in
      login_handler body request
    );
    Dream.get "/register" (fun _ -> Dream.html (Pages.registration_page ()));
    Dream.post "/register" (fun request -> 
      let%lwt body = Dream.body request in
      registeration_handler body request
    );
    Dream.get "/verify_email?token=:token" (fun request -> 
      let token = Dream.param request "token" in
      verify_email_handler token request
    );
    Dream.get "/static/**" (Dream.static "client")
  ]

let () = Lwt_main.run (start_server ())
