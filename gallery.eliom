{shared{
  open Eliom_pervasives
  open HTML5.M

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

module Auth = struct
  open Eliom_pervasives.XHTML.M
  open Lwt

let my_table = Eliom_state.create_volatile_table ()

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

let register () =
  Eliom_output.Xhtml.register ~service:connect_example connect_example_handler;
  Eliom_output.Action.register ~service:connect_action connect_action_handler

end

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

let () =
  Auth.register ()
  (* Gallery.register () *)

let bus = Eliom_bus.create ~name:"image_list" Json.t<messages>

let rgb_from_string color = (* color is in format "#rrggbb" *)
  let get_color i = (float_of_string ("0x"^(String.sub color (1+2*i) 2))) /. 255. in
  try get_color 0, get_color 1, get_color 2 with | _ -> 0.,0.,0.

let draw_server, image_string =
  let surface = Cairo.image_surface_create Cairo.FORMAT_ARGB32 ~width ~height in
  let ctx = Cairo.create surface in
  ((fun ((color : string), size, (x1, y1), (x2, y2)) ->

    (* Set thickness of brush *)
    Cairo.set_line_width ctx (float size) ;
    Cairo.set_line_join ctx Cairo.LINE_JOIN_ROUND ;
    Cairo.set_line_cap ctx Cairo.LINE_CAP_ROUND ;
    let red, green, blue =  rgb_from_string color in
    Cairo.set_source_rgb ctx ~red ~green ~blue ;

    Cairo.move_to ctx (float x1) (float y1) ;
    Cairo.line_to ctx (float x2) (float y2) ;
    Cairo.close_path ctx ;

    (* Apply the ink *)
    Cairo.stroke ctx ;
   ),
   (fun () ->
     let b = Buffer.create 10000 in
     (* Output a PNG in a string *)
     Cairo_png.surface_write_to_stream surface (Buffer.add_string b);
     Buffer.contents b
   ))


let main_service =
  My_appl.register_service ~path:[""] ~get_params:Eliom_parameters.unit
    (fun () () ->
      Lwt.return
      (List.map
         (fun dirname ->
           (div [div [h1 [pcdata dirname]];
                 div (Gallery.thumbnails "2011")]))
         (Gallery.dirnames "/home/spec/prog/worx/Current/ocsigen-auth/db")))

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

  let parse_pixels str =
    int_of_string (String.sub str 0 ((String.length str) - 2))
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

  let gen =
    let i = ref 0 in
    fun () -> incr i; !i
      
  open R
  type event = Start of float | Waiting | Progress of float

  let time = Rd.S.time ()
  let onmousemove el =
    let event, send = E.create () in
    el ## onmousemove <- Html.handler (fun ev ->
      send (S.value time);
      Js._false);
    send, event
  
  open React

  let rec wrap_element el =
    let duration = 0.6 in
    let f d = 0.-.cos (d /. duration *. 3.1415 *.2.) in
    let resize_div v =
      let v = f v in
      let size = 80.0 +. 60.0 *. ((v +. 1.0) *. 0.5) in
      ignore(div el ~size:(int_of_float size));
      () 
    in
    let send, mouse_down = onmousemove el in
    let elapsed_time = 
      E.filter
        (function Some _ -> true | None -> false)
        (E.diff (fun a b -> if a -. b > duration then Some a else None)
           mouse_down) in
    let elapsed_time = S.hold None elapsed_time in
    let delta = S.l2 (fun time start -> 
      match start with 
        | Some start -> let t = time -. start in t
        | None -> 0.0) time elapsed_time in
    let delta = S.filter ( (>) duration) 0. delta in
    S.map resize_div delta;
    (* this a nasty work around *)
    send (~-.duration);
    mouse_down

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
      (match !current_picture with
          Some pic -> let pic = pic ## replace_string (js"/thumbs", js"/mini") in div ## src <- pic;
        | None -> ());
      (* List.iter (fun c -> ignore(Dom.appendChild div (c :> Dom.node Js.t))) cs); *)
      (* img ## style ## height <- (js"100%"); *)
      div
    in
    (* let updater = Rd.S.time () in *)
    (*   Rd.appendChild (Html.document ## body) *)
    (*     (R.S.l1 (fun x -> *)
    (*       current_time := !current_time +. 10.; *)
    (*       let d = div *)
    (*         ~id:"tail" *)
    (*         ~color:"#FF0000" *)
    (*         ~backgroundColor:"#000000" *)
    (*         ~position:"absolute" *)
    (*         ~left:"605" *)
    (*         ~top:"3" *)
    (*         ~width:"605" *)
    (*         ~height:"605" *)
    (*         ~src:!current_picture *)
    (*         ~transluency:(js(let sec = !current_time/.1000.0 in if sec > 1.0 then "1.0"  *)
    (*           else string_of_float sec)) *)
    (*         ~padding:"10px" *)
    (*       in *)
    (*       let d = (d :> Dom.node Js.t) in *)
    (*       gen (), d) *)
    (*       updater); *)
    let divs = List.filter (fun el -> el ## className = js"picture") (elements_with_tag "div") in
    let _ = List.iter (fun el -> el ## style ## height <- (js"100%")) (elements_with_tag "img") in
    ignore (List.iter (fun el -> ignore(wrap_element el)) divs);
    for i = 0 to List.length divs - 1 do
      let x = i mod 7 in
      let y = i / 7 in
      let el = List.nth divs i in
      let top =  Js.string (string_of_int (y * 86+3)) in
      let left = Js.string (string_of_int (x * 86+3)) in
      el ## style ## top <- top;
      el ## style ## left <- left;
      el ## style ## position <- js "absolute";
      el ## style ## zIndex <- js "1";
    done;
    Html.document ## body ## style ## backgroundColor <- js"#fa8bf4";
    Js._false
 
  let _ = Dom_html.window##onload <- (Dom_html.handler onload)


  

      
}}
