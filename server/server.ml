open Lwt.Syntax

module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type

let () = Dotenv.export ()

type login_request = {
  email : string;
  password : string;
} [@@deriving yojson]

type registration_request = {
  name : string;
  email : string;
  password : string;
} [@@deriving yojson]

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
        | Ok (Some (_email, hashed_password)) ->
          let valid =
            match Utils.verify_password hashed_password password with
            | Ok verified -> verified
            | Error _ -> false in
              (* Dream.json ~status:`Unauthorized *)
                (* (Printf.sprintf {|{ "error": "The password does not match: %s" }|} (Argon2.ErrorCodes.message err)) in *)
              (* Dream.json ~status:`Unauthorized {|{ "error": "The password does not match" }|} in *)
                (* failwith ("Error while verifying password: " ^ Argon2.ErrorCodes.message err) in *)
          if valid then
            Dream.json
              (Printf.sprintf {|{ "status": "ok", "message": "Welcome %s" }|} email)
          else
            Dream.json ~status:`Unauthorized
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
          let* new_user = Utils.add_user db name email hashed_password in
          
          (match new_user with
          | Ok () ->
              Dream.json
                {|{ "status": "ok", "message": "User has been registered" }|}
          | Error e ->
              Dream.json ~status:`Internal_Server_Error
                (Printf.sprintf {|{ "error": "Failed to register user: %s" }|} (Caqti_error.show e)))
        | Ok (Some _) ->
          Dream.json ~status:`Unauthorized {|{ "error": "Email already exists" }|}
      )
    )

let get_db_url () =
  match Sys.getenv_opt "DATABASE_URL" with
  | Some url -> url
  | None -> failwith "DATABASE_URL not set"

let check_db_connection () =
  let open Lwt.Syntax in
  let db_uri = Uri.of_string (get_db_url ()) in
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
  let db_url = get_db_url () in
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
    Dream.get "/static/**" (Dream.static "client")
  ]

let () = Lwt_main.run (start_server ())
