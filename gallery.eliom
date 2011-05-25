{shared{
  open Eliom_pervasives
  open HTML5.M
  open Eliom_output.Html5

  let width = 700
  let height = 400
  let app_root_dir = "/home/spec/prog/worx/Current/gallery/"
  let abs_dir rel = app_root_dir ^ rel
  let is_real_name name =
  let char_at_end i c = 
    if String.length name - 1 - i >= 0 then name.[String.length name - 1 - i] == c 
    else true
  in
    not (char_at_end 1 '/' && char_at_end 0 '.' 
      || char_at_end 2 '/' && char_at_end 1 '.' && char_at_end 0 '.')

}}

module My_appl =
  Eliom_output.Eliom_appl (
    struct
      let application_name = "gallery"
      let params =
	{ Eliom_output.default_appl_params with
	  
          Eliom_output.ap_headers_before =
            [
              HTML5.M.link ~rel:[ `Stylesheet ]
                ~href:(HTML5.M.uri_of_string"./style.css")
                ();
            ];
	}

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

let connect_action =
  Eliom_services.post_coservice'
    ~name:"connect"
    ~post_params:(Eliom_parameters.string "login")
    ()

let login_box =
  post_form ~service:connect_action
    (fun loginname ->
      [p [pcdata "?";string_input
             ~input_type:`Password ~name:loginname ();]])

let connect_action_handler () login =
  Eliom_state.close_session () >>= fun () ->
  if login = "fancyfrancy" then
    Eliom_state.set_volatile_data  ~table:my_table login;
  return ()


module Gallery = struct
  open Lwt

let image_date filename =
  let open CalendarLib in
    (* let date_input = Unix.open_process_in (Printf.sprintf "jhead %s|grep \"Date/Time\" | sed -n 's/[^:]\\+: \\([^ ]\\+\\).*/\\1/p'" filename) in *)
      print_endline (filename ^ ".date");
      flush stdout;
      try
      let date_input = open_in (filename ^ ".date") in
      let date = input_line date_input in
      print_endline ("beta: " ^ filename ^ ".date");
      flush stdout;
      close_in date_input;
      let date = Printer.Date.from_fstring "%Y:%m:%d" date in
      Some date
      with | End_of_file -> None
        | Sys_error _ -> None

  let dirnames () =
    let static_dir = Eliom_services.static_dir () in
    let pic_dir = Eliom_output.Html5.make_string_uri ~service:static_dir ["db"] in
    let dir = Unix.opendir (abs_dir pic_dir) in
    let rec loop () =
      try
        let filename = Unix.readdir dir in
        if is_real_name filename then
          filename :: loop ()
        else loop ()
      with  _ -> []
    in
    let dirs = loop () in
    (* Unix.closedir dir; *)
    List.iter print_endline dirs;
    dirs

  let thumbnails gallery_name =
    let static_dir = Eliom_services.static_dir () in
    let pic_dir = Eliom_output.Html5.make_string_uri ~service:static_dir ["db";gallery_name;"thumbs";] in
    let dir = Unix.opendir (abs_dir pic_dir) in
    let rec loop acc =
      try
        let filename = Unix.readdir dir in
        if is_real_name filename then
           (if String.length filename > 5 then
              (if Str.last_chars filename 5 <> ".date" then
                  let date = image_date ((abs_dir pic_dir) ^ "/" ^  filename) in
                  loop ((date, filename) :: acc)
               else loop acc)
            else
               let date = image_date ((abs_dir pic_dir) ^ "/" ^  filename) in
               loop ((date, filename) :: acc))
        else loop acc
      with Unix.Unix_error _ -> acc
        | _ -> acc
    in
    (* Unix.closedir dir; *)
    let imgs = loop [] in
    let imgs = Array.of_list imgs in
    Array.sort (fun (date, filename) (date2, filename) ->
      match date, date2 with
        | Some date, Some date2 ->
          CalendarLib.Date.compare date date2
        | None,None -> 1) imgs;
    let imgs = Array.to_list imgs in
    (snd (List.fold_left (fun ((stamped, prev_date), acc) (date,filename) ->
      print_endline ("ala:" ^ filename ^ ".date");
      flush stdout;

          let date = image_date ((abs_dir pic_dir) ^ "/" ^  filename) in
          print_endline ("bela:" ^ filename ^ ".date");
          flush stdout;
          let date = match date with 
              Some date -> CalendarLib.Printer.Date.sprint "%d %B %Y (%A)" date 
            | None -> ""
          in
          let is_date = prev_date <> date in
          let is_date2 = is_date && (not stamped) in
          let stamped = false in
          let uri = (Eliom_output.Html5.make_uri
                       ~service:static_dir ["db";gallery_name;"thumbs";filename]) in
          let img1 =
            if is_date2 then
            [div ~a:[a_class ["date_overlay"]] [p [pcdata date];
                                               div ~a:[a_class ["picture_date"]]
                                                 [img  ~alt:date
                                                     ~src:"" ()]]]
          else [] in
          let img2 =
            div ~a:[a_class ["date_overlay"]] [p [pcdata date];
                                               div ~a:[a_class ["picture"]]
                                                 [img  ~alt:date
                                                     ~src:(uri) ()]] in
          (stamped, date), (img1@[img2]@acc)
    ) ((false, ""),[]) imgs))


end

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
    (* if el ## className <> js"picture_date" then *)
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

    
    let divs = List.filter (fun el -> el ## className = js"picture" || el ## className = js"picture_date") divs in
    let _ = List.iter (fun el -> 
      el ## style ## height <- (js"100%");
    ) imgs in
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
        let pic = (Js.Unsafe.coerce imgsa.(i) : Dom_html.imageElement Js.t) in
        let pic = pic ## src ## replace_string (js"/thumbs", js"/mini") in
        previews.(section) ## src <- pic;
      Js._false);

    done;
    (* Html.document ## body ## style ## backgroundColor <- js"#8f8f95"; *)
    Js._false
 
  let _ = Dom_html.window##onload <- (Dom_html.handler onload)
}}
let () =
    Eliom_output.Action.register ~service:connect_action connect_action_handler

let ala =
  My_appl.register_service ~path:[""] ~get_params:Eliom_parameters.unit
    (fun () () ->
      let sessdat = Eliom_state.get_volatile_data ~table:my_table () in
          Lwt.return
            (match sessdat with
              | Eliom_state.Data name ->
                (List.map
                   (fun dirname ->
                     let divs = Gallery.thumbnails dirname in
                     (div [div [h1 [pcdata dirname]];
                           div divs]))
                   (Gallery.dirnames ()))
              | Eliom_state.Data_session_expired
              | Eliom_state.No_data -> [login_box ()]))
