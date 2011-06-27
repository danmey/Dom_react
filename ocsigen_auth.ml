(* Simple login *)

open Eliom_pervasives.XHTML.M
open Lwt

(* "my_table" will be the structure used to store
   the session data (namely the login name): *)

let my_table = Eliom_state.create_volatile_table ()


(************************************************************)
(************ Connection of users, version 3 ****************)
(************************************************************)


(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let connect_example =
  Eliom_services.service
    ~path:["login"]
    ~get_params:Eliom_parameters.unit
    ()

let connect_action =
  Eliom_services.post_coservice'
    ~name:"connect"
    ~post_params:(Eliom_parameters.string "login")
    ()

(* As the handler is very simple, we register it now: *)
let disconnect_action =
  Eliom_output.Action.register_post_coservice'
    ~name:"disconnect"
    ~post_params:Eliom_parameters.unit
    (fun () () ->
      Eliom_state.close_session ())


(* -------------------------------------------------------- *)
(* login ang logout boxes:                                  *)

let disconnect_box s =
  Eliom_output.Xhtml.post_form ~service:disconnect_action
    (fun _ -> [p [Eliom_output.Xhtml.string_input
                    ~input_type:`Submit ~value:s ()]]) ()

let login_box =
  Eliom_output.Xhtml.post_form ~service:connect_action
    (fun loginname ->
      [p
          (let l = [pcdata "Login: ";
                    Eliom_output.Xhtml.string_input
                      ~input_type:`Password ~name:loginname ();]
         in l)
     ])
    


(* -------------------------------------------------------- *)
(* Handler for the "connect_example3" service (main page):    *)

let connect_example_handler () () =
  let sessdat = Eliom_state.get_volatile_data ~table:my_table () in
  return (html
       (head (title (pcdata "")) [])
       (body
          (match sessdat with
          | Eliom_state.Data name ->
              [p [pcdata ("Hello"); br ()];
              disconnect_box "Close session"]
          | Eliom_state.Data_session_expired
          | Eliom_state.No_data -> [login_box ()]
          )))


(* -------------------------------------------------------- *)
(* Handler for connect_action (user logs in):               *)

let connect_action_handler () login =
  Eliom_state.close_session () >>= fun () ->
  if login = "fancyfrancy" then
    Eliom_state.set_volatile_data  ~table:my_table login;
  return ()

module Gallery = struct
  open Eliom_services
  let service =
    Eliom_services.service
      ~path:["show"]
      ~get_params:Eliom_parameters.unit
      ()
      
  let dirnames dirname =
    let dir = Unix.opendir dirname in
    let rec loop () =
      try
        let name = Unix.readdir dir in
        if name <> "." && name <> ".." then name :: loop ()
        else loop ()
      with _ -> []
    in
    let dirs = loop () in
    List.iter print_endline dirs;
    dirs

  let thumbnails dirname =
    let dir = Unix.opendir ("/home/spec/prog/worx/Current/ocsigen-auth/db/" ^ dirname ^ "/thumbnails") in
      let rec table_row trows =
        let rec cell i acc =
          try
            let filename = Unix.readdir dir in
            let img = 
              img ~alt:filename
                ~src:(Eliom_output.Html5.make_uri
                        ~service:(Eliom_services.static_dir ()) ["db";dirname;"thumbnails";filename]) () in
            let thumbnail = td [div [img]] in
            let acc = thumbnail :: acc in
            if i = 15 then
              table_row ((tr (List.hd acc) (List.tl acc)) :: trows)
            else
              cell (i+1) acc
          with _ -> trows
        in
        cell 0 [] in
      let rows = table_row [] in
      [table (List.hd rows) (List.tl rows)]

  let handler () () =
    let sessdat = Eliom_state.get_volatile_data ~table:my_table () in
    return (html
              (head 
                 (title (pcdata ""))           
                 [style ~contenttype:"text/css"
                     [cdata_style
                         "body
{
    background-color:#afafff;
}"]])
              (body
                 (match sessdat with
                   | Eliom_state.Data name ->
                     List.map
                       (fun dirname ->
                         (div [div [h1 [pcdata dirname]];
                         div (thumbnails dirname)])) (dirnames "/home/spec/prog/worx/Current/ocsigen-auth/db")
                   | Eliom_state.Data_session_expired
                   | Eliom_state.No_data -> [login_box ()]
                 )))
      
  let register () =
    Eliom_output.Xhtml.register ~service:service handler;

end

(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_output.Xhtml.register ~service:connect_example connect_example_handler;
  Eliom_output.Action.register ~service:connect_action connect_action_handler;
  Gallery.register ()
