open Lwt.Infix

module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type

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
