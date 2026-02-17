open Argon2
open Lwt.Infix

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

let get_email_key =
  match Sys.getenv_opt "BREVO_KEY" with
  | Some url -> url
  | None -> failwith "BREVO_KEY not set"

let send_email ~to_email ~to_name ~subject ~html_content =
  (* let uri = Uri.of_string "https://api.brevo.com/v3/smtp/email" in *)
  let uri = Uri.of_string "https://api.brevo.com/v3/emailCampaigns" in
  let api_key = get_email_key in

  let body =
    `Assoc [
      "name", `String "OCaml Auth App";
      "subject", `String subject;
      "sender", `Assoc [
          ("name", `String "OCaml Auth App");
          ("email", `String "noreply@authsys.com")
      ];
      "type", `String "classic";
      "htmlContent", `String html_content;
      "recipients",
        `List [
          `Assoc [
            ("email", `String to_email);
            ("name", `String to_name)
          ]
        ];
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
