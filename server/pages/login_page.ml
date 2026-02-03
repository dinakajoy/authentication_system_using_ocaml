open Tyxml.Html

let login_page () =
  html
    (head 
      (title (txt "Login Page"))
      [
        meta ~a:[a_charset "UTF-8"] ();
        meta ~a:[ a_name "viewport"; a_content "width=device-width, initial-scale=1.0" ] ();
        (* Link Tailwind via CDN *)
        script ~a:[ a_src "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4" ] (txt "");
        
        (* Link JavaScript file *)
        script ~a:[ a_src "/static/main.js"; a_defer () ] (txt "")
      ]
    )
    (body
       ~a:[ a_class 
        [
          "min-h-screen"; "flex"; "flex-col"; "items-center"; "justify-center"; "bg-gray-100"; "p-6"; "text-gray-600"
        ]
      ]
      [
        form
          ~a:[
            a_id "login-form";
            a_class ["bg-white"; "p-12"; "rounded-md"; "shadow-md"; "w-full"; "max-w-md"]
          ]
          [
            h2 ~a:[a_class ["text-2xl"; "font-bold"; "mb-6"; "text-center"; "underline"]] [ txt "Login" ];
 
            (* Email Field *)
            Snippets.email_input;
 
            (* Password Field *)
            Snippets.password_input;

            (* Error Field *)
            Snippets.error_div;

            (* Status Field *)
            Snippets.status_div;
 
            (* Submit Button *)
            div ~a:[ a_class [ "mt-6" ] ]
              [
                button
                  ~a:[
                    a_button_type `Submit;
                    a_id "login-button";
                    a_class [
                      "bg-blue-500";
                      "hover:bg-blue-600";
                      "text-white";
                      "font-semibold";
                      "py-3"; "px-4"; "rounded";
                      "w-full";
                      "transition-colors"; "duration-200";
                      "disabled:opacity-50"; "disabled:cursor-not-allowed"
                    ]
                  ]
                  [ txt "Login" ]
              ];

            div ~a:[ a_class [ "my-2"; "flex"; "gap-2"; "justify-end" ] ] [
              p ~a:[
                a_class [
                  "text-gray-500"; "hover:text-gray-600"; "font-semibold"
                ]
              ]
              [ txt "Don't have an account?"];
              a ~a:[
                a_class [
                  "text-blue-500"; "hover:text-blue-600"; "font-semibold"
                ];
                a_href "/register";
              ]
              [ txt "Register here" ]
            ]
          ];
      ]
    )
    |> Format.asprintf "%a" (Tyxml.Html.pp ())
 