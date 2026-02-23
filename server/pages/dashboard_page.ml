open Tyxml.Html

let dashboard_page () =
  html
    (head 
      (title (txt "Dashboard Page"))
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
        div
          ~a:[ a_class ["bg-white"; "p-12"; "rounded-md"; "shadow-md"; "w-full"; "max-w-md"] ]
          [
            h2 ~a:[a_class ["text-2xl"; "font-bold"; "mb-6"; "text-center"; "underline"]] [ txt "Welcome to your Dashboard!" ];
            p [ txt "This is a protected page that only authenticated users can access." ];
            a ~a:[ a_href "/logout"; a_class ["mt-6"; "block"; "text-center"; "text-blue-500"; "hover:underline"] ] [ txt "Logout"]
          ]
      ]
    )
    |> Format.asprintf "%a" (Tyxml.Html.pp ())
 