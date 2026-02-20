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
      a_class ["w-full"; "border"; "border-gray-300"; "p-2"; "rounded"]
    ] ();
  ]

let email_input =
   div ~a:[a_class ["mb-6"]] [
    label ~a:[ a_label_for "email"; a_class ["block"; "font-medium"; "mb-1"] ] [ txt "Email" ];
    input ~a:[
      a_input_type `Email;
      a_name "email";
      a_id "email";
      a_placeholder "you@example.com";
      a_required ();
      a_class ["w-full"; "border"; "border-gray-300"; "p-2"; "rounded"]
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
      a_class ["w-full"; "border"; "border-gray-300"; "p-2"; "rounded"]
    ] ();
  ]

let confirm_password_input = 
  div ~a:[a_class ["mb-6"]] [
    label ~a:[ a_label_for "confirm_password"; a_class ["block"; "font-medium"; "mb-1"] ] [ txt "Confirm Password" ];
    input ~a:[
      a_input_type `Password;
      a_name "confirm_password";
      a_id "confirm_password";
      a_placeholder "••••••••";
      a_required ();
      a_class ["w-full"; "border"; "border-gray-300"; "p-2"; "rounded"]
    ] ();
  ]

let error_div =
  div
    ~a:[
      a_id "form-error";
      a_class [
        "hidden";
        "mb-4";
        "text-red-600";
        "bg-red-100";
        "border";
        "border-red-300";
        "p-3";
        "rounded";
        "text-sm"
      ]
    ]
    [ txt "" ]

let status_div =
  div
    ~a:[
      a_id "form-status";
      a_class [
        "hidden";
        "mb-4";
        "text-green-600";
        "bg-green-100";
        "border";
        "border-green-300";
        "p-3";
        "rounded";
        "text-sm"
      ]
    ]
    [ txt "" ]