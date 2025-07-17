open Tyxml.Html

let not_found_page () =
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
    |> Format.asprintf "%a" (Tyxml.Html.pp ())
