{shared{
  open Eliom_pervasives
  open HTML5.M
  open Eliom_output.Html5

  let width = 700
  let height = 400
}}

module My_appl =
  Eliom_output.Eliom_appl (
    struct
      let application_name = "gallery"
      let params = Eliom_output.default_appl_params
    end)

{client{
  open Event_arrows
  let draw ctx (color, size, (x1, y1), (x2, y2)) =
    ctx##strokeStyle <- (Js.string color);
    ctx##lineWidth <- float size;
    ctx##beginPath();
    ctx##moveTo(float x1, float y1);
    ctx##lineTo(float x2, float y2);
    ctx##stroke()
}}

{shared{
  type messages = string list deriving (Json)
}}

(* module Auth = struct *)
  open Lwt

let my_table = Eliom_state.create_volatile_table ()

let connect_example =
  Eliom_services.service
    ~path:[""]
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
  post_form ~service:disconnect_action
    (fun _ -> [p [string_input
                    ~input_type:`Submit ~value:s ()]]) ()

let login_box =
  post_form ~service:connect_action
    (fun loginname ->
      [p [pcdata "?";string_input
             ~input_type:`Password ~name:loginname ();]])
    


(* -------------------------------------------------------- *)
(* Handler for the "connect_example3" service (main page):    *)

let gallery_link =
  a
    ~service:(Eliom_services.external_service
       ~prefix:"http://fr.wikipedia.org"
       ~path:["wiki";""]
       ~get_params:Eliom_parameters.unit
   ()) [] ()

let connect_example_handler () () =
  let sessdat = Eliom_state.get_volatile_data ~table:my_table () in
  return (html
       (head (title (pcdata "")) [])
       (body
          (match sessdat with
          | Eliom_state.Data name ->
            [p [gallery_link]]
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


(* end *)

module Gallery = struct
  open Lwt
      
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
    let dir = Unix.opendir ("/home/spec/prog/worx/Current/ocsigen-auth/db/" ^ dirname ^ "/thumbs") in
        let rec loop acc =
          try
            let filename = Unix.readdir dir in
            let img =
              div ~a:[a_class ["picture"]]
              [img  ~alt:filename
                ~src:(Eliom_output.Html5.make_uri
                        ~service:(Eliom_services.static_dir ()) ["db";dirname;"thumbs";filename]) ()] in
            loop (img :: acc)
          with _ -> acc
        in
        loop []
end

(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

(* let () = *)
(*   Auth.register () *)
  (* Gallery.register () *)

let main_service =
  My_appl.register_service ~path:[""] ~get_params:Eliom_parameters.unit
    (fun () () ->
      let sessdat = Eliom_state.get_volatile_data ~table:my_table () in
          Lwt.return
            (match sessdat with
              | Eliom_state.Data name ->
                (List.map
                   (fun dirname ->
                     (div [div [h1 [pcdata dirname]];
                           div (Gallery.thumbnails "2011")]))
                   (Gallery.dirnames "/home/spec/prog/worx/Current/ocsigen-auth/db"))
              | Eliom_state.Data_session_expired
              | Eliom_state.No_data -> [login_box ()]))


 



{client{
module Html = Dom_html
module Dom = Dom
module Rd = React_dom
module R = Lwt_react
let js = Js.string

let current_picture : Js.js_string Js.t option ref = ref None
let opacity_time = ref 0.0
let current_time = ref 0.0

  let elements_with_tag tag =
    let arr = Dom_html.document ## getElementsByTagName (js tag) in
    let rec loop i acc =
      if i < arr ## length then
        let item = arr ## item (i) in
        Js.Optdef.case item (fun () -> loop (i+1) acc) (fun el -> loop (i+1) (el :: acc))
      else acc in
    loop 0 []

  let div el ~size =
    let prev_w = el ## clientWidth in
    let prev_h = el ## clientHeight in
    let top = el ## offsetTop in
    let left = el ## offsetLeft in
    let delta = size - prev_w in
    let fixup = delta / 2 in
    let top =  Js.string (string_of_int (top - fixup)) in
    let left = Js.string (string_of_int (left - fixup)) in
    let size = string_of_int size in
    el ## style ## width <- js size;
    el ## style ## height <- js size;
    el ## style ## top <- top;
    el ## style ## left <- left;
    el

  open React

  let time = Rd.S.time ()

  let onmousemove el =
    let event, send = E.create () in
    el ## onmousemove <- Html.handler (fun ev ->
      send (S.value time);
      Js._false);
    send, event

  let onmousedown el =
    let event, send = E.create () in
    el ## onmousedown <- Html.handler (fun ev ->
      send (S.value time);
      Js._false);
    event
  
  let rec wrap_element el =
    let duration = 0.6 in
    let f d = 0.-.cos (d /. duration *. 3.1415 *.2.) in
    let resize_div v =
      let v = f v in
      let size = 80.0 +. 60.0 *. ((v +. 1.0) *. 0.5) in
      ignore(div el ~size:(int_of_float size));
      () 
    in
    let send, mouse_over = onmousemove el in
    let mouse_down = onmousedown el in
    let elapsed_time0 = 
      E.filter
        (function Some _ -> true | None -> false)
        (E.diff (fun a b -> if a -. b > duration then Some a else None)
           mouse_over) in
    let elapsed_time = S.hold None elapsed_time0 in
    let delta = S.l2 (fun time start -> 
      match start with 
        | Some start -> let t = time -. start in t
        | None -> 0.0) time elapsed_time in
    let delta = S.filter ( (>) duration) 0. delta in
    S.map resize_div delta;
    (* this a nasty work around *)
    send (~-.duration);
    ()

  let onload ev =

    let div ~id ~color ~backgroundColor ~position ~padding ~left ~top ~width ~height ~src ~transluency =
      let div = Html.createImg Html.document in
      div ## setAttribute (js"id", js id);
      div ## style ## color <- js color;
      div ## style ## backgroundColor <- js backgroundColor;
      div ## style ## position <- js position;
      div ## style ## padding <- js padding;
      div ## style ## left <- js left;
      div ## style ## top <- js top;
      div ## style ## width <- js width;
      div ## style ## height <- js height;
      div ## style ## opacity <- Js.Optdef.return transluency;
      (* (match !current_picture with *)
      (*     Some pic -> let pic = pic ## replace_string (js"/thumbs", js"/mini") in div ## src <- pic; *)
      (*   | None -> ()); *)
      div ## src <- src;
      div
    in
    let gen =
      let i = ref 0 in
      fun () -> incr i; !i in
    let preview row src =
      let img = (div
        ~id:"tail"
        ~color:"#000000"
        ~backgroundColor:"#000000"
        ~position:"absolute"
        ~left:"605"
        ~top:(string_of_int (3 + (602) * row))
        ~width:"605"
        ~height:"605"
        ~src:(js src)                
        ~transluency:(js"1.0")
        ~padding:"10px") in
      Dom.appendChild (Html.document ## body) img;
      img
    in
    let imgs = elements_with_tag "img" in
    let divs = elements_with_tag "div" in
    let imgsa = Array.of_list imgs in
    let previews = Array.init 4 (fun i ->
      let pic = (Js.Unsafe.coerce imgsa.(i) : Dom_html.imageElement Js.t) ## src in
      let pic = Js.to_string (pic ## replace_string (js"/thumbs", js"/mini")) in
      Html.document ## title <- js pic;
      preview i pic) in

    
    let divs = List.filter (fun el -> el ## className = js"picture") divs in
    let _ = List.iter (fun el -> el ## style ## height <- (js"100%")) imgs in
    List.iter wrap_element divs;
    for i = 0 to List.length divs - 1 do
      let x = i mod 7 in
      let y = i / 7 in
      let section = y / 7 in
      let el = List.nth divs i in
      let top =  Js.string (string_of_int (y * 86+3)) in
      let left = Js.string (string_of_int (x * 86+3)) in
      el ## style ## top <- top;
      el ## style ## left <- left;
      el ## style ## position <- js "absolute";
      el ## style ## zIndex <- js "1";
      el ## onmousedown <- Html.handler (fun ev ->
        let pic = (Js.Unsafe.coerce imgsa.(i) : Dom_html.imageElement Js.t) ## src in
        let pic = pic ## replace_string (js"/thumbs", js"/mini") in
        previews.(section) ## src <- pic;
      Js._false);

    done;
    Html.document ## body ## style ## backgroundColor <- js"#8f8f95";
    Js._false
 
  let _ = Dom_html.window##onload <- (Dom_html.handler onload)
}}
let () =
  (* Eliom_output.Xhtml.register ~service:connect_example connect_example_handler; *)
  Eliom_output.Action.register ~service:connect_action connect_action_handler
