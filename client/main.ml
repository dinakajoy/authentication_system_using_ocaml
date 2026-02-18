open Js_of_ocaml
open Brr
open Fut.Syntax
open Brr_io
module Html = Dom_html

let get_element_by_id id = (Document.find_el_by_id G.document) (Jstr.v id)

let is_loading el_id state =
  let btn = get_element_by_id el_id in
    match btn with
    | Some el -> 
      if state
        then 
          (El.set_prop (El.Prop.bool (Jstr.v "disabled")) true el;
          El.set_prop (El.Prop.jstr (Jstr.v "innerHTML")) (Jstr.v "Loading...") el)
      else
        (El.set_prop (El.Prop.bool (Jstr.v "disabled")) false el;
        El.set_prop (El.Prop.jstr (Jstr.v "innerHTML")) (Jstr.v "Submit") el)
    | None -> ()

let display_error state error = 
  let error_element =  (Document.find_el_by_id G.document) (Jstr.v "form-error") in
  match error_element with
  | Some v -> 
    El.set_prop (El.Prop.jstr (Jstr.v "innerHTML")) (Jstr.v error) v;
    if state 
      then El.set_inline_style El.Style.display (Jstr.v "block") v
    else El.set_inline_style El.Style.display (Jstr.v "none") v;
    is_loading "login-button" false
  | None -> ()

let display_status state status = 
  let status_element =  (Document.find_el_by_id G.document) (Jstr.v "form-status") in
  match status_element with
  | Some v -> 
    El.set_prop (El.Prop.jstr (Jstr.v "innerHTML")) (Jstr.v status) v;
    if state 
      then El.set_inline_style El.Style.display (Jstr.v "block") v
    else El.set_inline_style El.Style.display (Jstr.v "none") v
  | None -> ()

let extract_message json_str el =
  try
    let json = Yojson.Basic.from_string json_str in
    match json with
    | `Assoc fields -> (
        match List.assoc_opt "message" fields with
        | Some (`String msg) -> 
          Console.log [ Jstr.v "Success!", msg ];
          display_status true msg;
          is_loading el false
        | _ -> (
            match List.assoc_opt "error" fields with
            | Some (`String err) -> 
              Console.log [ Jstr.v "Error!", err ];
              display_error true err;
              is_loading el false
            | _ -> 
              display_error true "Something went wrong.";
              is_loading el false
          )
      )
      
    | _ -> ()
  with _ -> ()

let get_response_data response el =
  let* data = Fetch.Body.text (Fetch.Response.as_body response) in
  match data with
  | Ok response -> 
    Fut.return (extract_message (Jstr.to_string response) el)
  | Error error -> 
    Console.error [ Jstr.v "Error!", Jv.Error.message error ];
    let () = display_error true "Something went wrong!" in
    Fut.return ()

let make_request url data el = 
  display_status false "";
  display_error false "";
  let init =
    Fetch.Request.init
      ~method':(Jstr.of_string "POST")
      ~body:(Fetch.Body.of_jstr data)
      ~headers:
        (Fetch.Headers.of_assoc
           [ Jstr.of_string "Content-Type", Jstr.of_string "application/json" ])
      ()
  in
  let* result = Fetch.url ~init url in
  match result with
  | Ok response -> 
    get_response_data response el
  | Error error -> 
    Console.error [ Jstr.v "Err!", Jv.Error.message error ];
    is_loading el false;
    let () = display_error true "Something went wrong!" in
    Fut.return ()

let get_input_value_by_id id =
  match Document.find_el_by_id G.document (Jstr.v id) with
  | Some el -> 
    let input_value = Jstr.to_string (El.prop El.Prop.value el) in
    if (Jstr.is_empty (Jstr.of_string input_value)) 
      then Jstr.v ""
    else 
      Jstr.v input_value
  | None -> Jstr.v ""

let on_register_handler () =
  let () = is_loading "registration-button" true in
  let name = get_input_value_by_id "name" |> Jstr.to_string in
  let email = get_input_value_by_id "email" |> Jstr.to_string in
  let password = get_input_value_by_id "password" |> Jstr.to_string in
  if name = "" || email = "" || password = ""
    then display_error true "Please fill all fields"
  else 
    display_error false "";
    let json =
      `Assoc [
        "name", `String name;
        "email", `String email;
        "password", `String password;
      ]
    in
    let data_as_json = Yojson.Basic.to_string json in
    make_request ("http://localhost:8080/register" |> Jstr.of_string) (data_as_json |> Jstr.of_string) "registration-button"

let on_login_handler () =
  let () = is_loading "login-button" true in
  let email = get_input_value_by_id "email" |> Jstr.to_string in
  let password = get_input_value_by_id "password" |> Jstr.to_string in
  if email = "" || password = ""
    then display_error true "Please fill all fields"
  else 
    display_error false "";
    let json =
      `Assoc [
        "email", `String email;
        "password", `String password;
      ]
    in
    let data_as_json = Yojson.Basic.to_string json in
    make_request ("http://localhost:8080/login" |> Jstr.of_string) (data_as_json |> Jstr.of_string) "login-button"

let start =
  let login_form = get_element_by_id "login-form" in
    match login_form with
    | Some el -> 
      ignore(Ev.listen Brr_io.Form.Ev.submit (fun ev -> 
        Ev.prevent_default ev;
        Console.log [Jstr.v "Login form submitted"];
        ignore (on_login_handler ())
      ) (El.as_target el))
    | None -> Console.log [Jstr.v "Login form not found"];
  let register_form = get_element_by_id "registration-form" in
    match register_form with
    | Some el -> 
      ignore(Ev.listen Brr_io.Form.Ev.submit (fun ev -> 
        Ev.prevent_default ev;
        Console.log [Jstr.v "Registration form submitted"];
        ignore (on_register_handler ())
      ) (El.as_target el))
    | None -> Console.log [Jstr.v "Registration form not found"]

let () = Html.window##.onload := Html.handler (fun _ -> start; Js._true)
