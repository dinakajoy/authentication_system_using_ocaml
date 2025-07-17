module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type

let () = Dotenv.export () 

type login_request = {
  email : string;
  password : string;
} [@@deriving yojson]

let login_handler login_body request =
  match Yojson.Safe.from_string login_body with
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
          let valid = password = Utils.verify_password password hashed_password in
          if valid then
            Dream.json
              (Printf.sprintf {|{ "status": "ok", "message": "Welcome %s" }|} email)
          else
            Dream.json ~status:`Unauthorized
              {|{ "error": "Invalid email or password" }|}
      )
    )

let () =
  let db_url =
    match Sys.getenv_opt "DATABASE_URL" with
    | Some url -> url
    | None -> failwith "DATABASE_URL not set"
  in
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.sql_pool db_url
  @@ Dream.router [
    Dream.get "/" (fun _ -> Dream.html (Pages.login_page ()));
    Dream.get "/login" (fun _ -> Dream.html (Pages.login_page ()));
    Dream.post "/login" (fun request -> 
      let%lwt body = Dream.body request in
      login_handler body request
    );
    Dream.get "/register" (fun _ -> Dream.html (Pages.registration_page ()))
  ]
