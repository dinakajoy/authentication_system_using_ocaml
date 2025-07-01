(* open Lwt.Infix *)
open Cohttp
open Cohttp_lwt_unix
open Tyxml.Html

let name_input = 
   div ~a:[a_class ["mb-6"]] [
    label ~a:[ a_label_for "name"; a_class ["block"; "font-medium"; "mb-1"] ] [ txt "Name" ];
    input ~a:[
      a_input_type `Text;
      a_name "name";
      a_id "name";
      a_placeholder "Your full name";
      a_required ();
      a_class ["w-full"; "border"; "p-2"; "rounded"]
    ] ();
  ]

let email_input =
   (* Email Field *)
   div ~a:[a_class ["mb-6"]] [
    label ~a:[ a_label_for "email"; a_class ["block"; "font-medium"; "mb-1"] ] [ txt "Email" ];
    input ~a:[
      a_input_type `Email;
      a_name "email";
      a_id "email";
      a_placeholder "you@example.com";
      a_required ();
      a_class ["w-full"; "border"; "p-2"; "rounded"]
    ] ();
  ]

let password_input = 
  div ~a:[a_class ["mb-6"]] [
    label ~a:[ a_label_for "password"; a_class ["block"; "font-medium"; "mb-1"] ] [ txt "Password" ];
    input ~a:[
      a_input_type `Password;
      a_name "password";
      a_id "password";
      a_placeholder "••••••••";
      a_required ();
      a_class ["w-full"; "border"; "p-2"; "rounded"]
    ] ();
  ]

let registration_page =
  html
    (head 
      (title (txt "Registration Page"))
      [
        meta ~a:[a_charset "UTF-8"] ();
        meta ~a:[ a_name "viewport"; a_content "width=device-width, initial-scale=1.0" ] ();
        (* Link Tailwind via CDN *)
        script ~a:[ a_src "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4" ] (txt "")
      ]
    )
    (body
       ~a:[ a_class 
        [
          "min-h-screen"; "flex"; "flex-col"; "items-center"; "justify-center"; "bg-gray-100"; "p-6"; "text-gray-600"
        ]
      ]
      [form
        ~a:[ a_action "/register"; a_method `Post; a_class 
          [
            "bg-gray-200"; "p-12"; "rounded-md"; "shadow-md"; "w-full"; "max-w-md"
          ]
        ]
        [
          h2 ~a:[a_class ["text-2xl"; "font-bold"; "mb-6"; "text-center"; "underline"]] [ txt "Register" ];

          (* Name Field *)
          name_input;

          (* Email Field *)
          email_input;

          (* Password Field *)
          password_input;

          (* Submit Button *)
          div ~a:[ a_class [ "mt-6" ] ]
            [
              button
                ~a:[
                  a_button_type `Submit;
                  a_class [
                    "bg-blue-600"; "hover:bg-blue-700";
                    "text-white"; "font-semibold";
                    "py-2"; "px-4"; "rounded";
                    "w-full"; "transition-colors"; "duration-200"
                  ]
                ]
                [ txt "Register" ]
            ];
        ]
      ]
    )

let login_page =
  html
    (head 
      (title (txt "Login Page"))
      [
        meta ~a:[a_charset "UTF-8"] ();
        meta ~a:[ a_name "viewport"; a_content "width=device-width, initial-scale=1.0" ] ();
        (* Link Tailwind via CDN *)
        script ~a:[ a_src "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4" ] (txt "")
      ]
    )
    (body
       ~a:[ a_class 
        [
          "min-h-screen"; "flex"; "flex-col"; "items-center"; "justify-center"; "bg-gray-100"; "p-6"; "text-gray-600"
        ]
      ]
      [form
        ~a:[ a_action "/login"; a_method `Post; a_class 
          [
            "bg-gray-200"; "p-12"; "rounded-md"; "shadow-md"; "w-full"; "max-w-md"
          ]
        ]
        [
          h2 ~a:[a_class ["text-2xl"; "font-bold"; "mb-6"; "text-center"; "underline"]] [ txt "Login" ];

          (* Email Field *)
          email_input;

          (* Password Field *)
          password_input;

          (* Submit Button *)
          div ~a:[ a_class [ "mt-6" ] ]
            [
              button
                ~a:[
                  a_button_type `Submit;
                  a_class [
                    "bg-blue-600"; "hover:bg-blue-700";
                    "text-white"; "font-semibold";
                    "py-2"; "px-4"; "rounded";
                    "w-full"; "transition-colors"; "duration-200"
                  ]
                ]
                [ txt "Login" ]
            ];
        ]
      ]
    )

let not_found_page : [> Html_types.html ] elt =
  html
    (head
       (title (txt "404 - Page Not Found"))
       [ 
        meta ~a:[ a_charset "UTF-8" ] ();
        meta ~a:[ a_name "viewport"; a_content "width=device-width, initial-scale=1.0" ] ();
        (* Link Tailwind via CDN *)
        script ~a:[ a_src "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4" ] (txt "")
      ]
    )
    (body
      ~a:[ a_class 
        [
          "min-h-screen"; "flex"; "flex-col"; "items-center"; "justify-center"; "bg-gray-100"; "text-center"; "p-6"
        ]
      ]
      [
        h1 ~a:[ a_class [ "text-6xl"; "font-bold"; "text-red-600"; "mb-4" ] ] [ txt "404" ];
        p ~a:[ a_class [ "text-lg"; "text-gray-700"; "mb-6" ] ] [
          txt "Sorry, the page you are looking for does not exist."
        ];
        a ~a:[
          a_href "/";
          a_class [
            "inline-block"; "bg-blue-600"; "text-white"; "px-4"; "py-2";
            "rounded"; "hover:bg-blue-700"; "transition-colors"; "duration-200"
          ]
        ] [ txt "Go back to homepage" ]
      ]
    )

let register_body = Format.asprintf "%a" (pp_elt ()) registration_page
let login_body = Format.asprintf "%a" (pp_elt ()) login_page
let not_found_body = Format.asprintf "%a" (pp ()) not_found_page

let handle_request uri =
  match uri with
  | "" | "/" -> login_body
  | "/register" | "/register/" -> register_body
  | "/login" | "/login/" -> login_body
  | _ -> not_found_body

let () =
  let callback _conn req _body =
    let uri = req |> Request.resource in
    let response_body = handle_request uri in
    Server.respond_string ~status:`OK ~body:response_body 
      ~headers:(Header.init_with "Content-Type" "text/html") ()
  in
  let server = Server.make ~callback () in
  let port = 8000 in
  let mode = `TCP (`Port port) in
  Format.printf "listening on http://localhost:%d\n%!" port;
  Server.create ~mode server |> Lwt_main.run