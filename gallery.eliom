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
              table_row ((tr (List.tl acc)) :: trows)
            else
              cell (i+1) acc
          with _ -> trows
        in
        cell 0 [] in
      let rows = table_row [] in
      [table (List.hd rows) (List.tl rows)]

(*   let handler () () = *)
(*     let sessdat = Eliom_state.get_volatile_data ~table:Auth.my_table () in *)
(*     return (html *)
(*               (head  *)
(*                  (title (pcdata ""))            *)
(*                  [style ~contenttype:"text/css" *)
(*                      [cdata_style *)
(*                          "body *)
(* { *)
(*     background-color:#afafff; *)
(* }"]]) *)
(*               (body *)
(*                  (match sessdat with *)
(*                    | Eliom_state.Data name -> *)
(*                      List.map *)
(*                        (fun dirname -> *)
(*                          (div [div [h1 [pcdata dirname]]; *)
(*                          div (thumbnails dirname)])) (dirnames "/home/spec/prog/worx/Current/ocsigen-auth/db") *)
(*                    | Eliom_state.Data_session_expired *)
(*                    | Eliom_state.No_data -> [Auth.login_box ()] *)
(*                  ))) *)
      
  (* let register () = *)
  (*   Eliom_output.Xhtml.register ~service:service handler; *)

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

(* let _ = Lwt_stream.iter draw_server (Eliom_bus.stream bus) *)

let picture_list_service =
  Eliom_output.Text.register_service
    ~path:["picture_list"]
    ~get_params:Eliom_parameters.unit
    (fun () () -> Lwt.return ("db/2011/thumbnails/DSC00253.jpg", "image/jpg"))

let main_service =
  My_appl.register_service ~path:[""] ~get_params:Eliom_parameters.unit
    (fun () () ->
       (* Eliom_services.onload *)
       (*   {{ *)
       (*     (\* let canvas = Dom_html.createCanvas Dom_html.document in *\) *)
       (*     (\* let ctx = canvas##getContext (Dom_html._2d_) in *\) *)
       (*     (\* canvas##width <- width; canvas##height <- height; *\) *)
       (*     (\* ctx##lineCap <- Js.string "round"; *\) *)

       (*     (\* Dom.appendChild Dom_html.document##body canvas; *\) *)

       (*     (\* (\\* The initial image: *\\) *\) *)
       (*     (\* let img = Dom_html.createImg Dom_html.document in *\) *)
       (*     (\* img##alt <- Js.string "canvas"; *\) *)
       (*     (\* img##src <- Js.string (Eliom_output.Html5.make_string_uri ~service:%picture_list_service ()); *\) *)
       (*     (\* img##onload <- Dom_html.handler (fun ev -> ctx##drawImage(img, 0., 0.); Js._false); *\) *)

       (*     (\* let x = ref 0 and y = ref 0 in *\) *)
       (*     (\* let set_coord ev = *\) *)
       (*     (\*   let x0, y0 = Dom_html.elementClientPosition canvas in *\) *)
       (*     (\*   x := ev##clientX - x0; y := ev##clientY - y0 in *\) *)
       (*     (\* let compute_line ev = *\) *)
       (*     (\*   let oldx = !x and oldy = !y in *\) *)
       (*     (\*   set_coord ev; *\) *)
       (*     (\*   ("#ff9933", 5, (oldx, oldy), (!x, !y)) *\) *)
       (*     (\* in *\) *)
       (*     (\* let (bus:messages Eliom_bus.t) = %bus in *\) *)
       (*     (\* let line ev = *\) *)
       (*     (\*   let v = compute_line ev in *\) *)
       (*     (\*   (\\* let _ = Eliom_bus.write bus v in *\\) *\) *)
       (*     (\*   draw ctx v *\) *)

       (*     (\* in *\) *)
       (*     (\* let imgs = Dom_html.document ## getElementsByTagName (Js.string "img") in *\) *)
       (*     (\* (Js.Optdef.iter (imgs ## item (0)) (fun f -> Dom_html.window ## alert (f ## tagName))); *\) *)
       (*     let _ = Dom_html.window ## alert (Js.string "nice") in *)
       (*     () *)
       (*      (\* let _ = Lwt_stream.iter (draw ctx) (Eliom_bus.stream bus) in *\) *)
       (*     (\* ignore (run (mousedowns canvas *\) *)
       (*     (\*                (arr (fun ev -> set_coord ev; line ev) *\) *)
       (*     (\*                 >>> first [mousemoves Dom_html.document (arr line); *\) *)
       (*     (\*                            mouseup Dom_html.document >>> (arr line)])) ()); *\) *)
       (*   }}; *)
      Lwt.return [])
(* []p [pcdata "Reactive programming"]]) *)
        (* (List.map *)
        (*    (fun dirname -> *)
        (*      (div [div [h1 [pcdata dirname]]; *)
        (*            div (Gallery.thumbnails "2011")])) *)
        (*    (Gallery.dirnames "/home/spec/prog/worx/Current/ocsigen-auth/db"))) *)
{client{
  open Lwt_react
  (* let _ = *)
  (*   let signal, stop = React_dom.ticks 0.1 in *)
  (*   let pr_x = S.map  *)
  (*     (fun x -> *)
  (*       Dom_html.window ## alert (Js.string (string_of_float x)); *)
  (*       stop () *)
  (*     ) signal in *)

  (*   Dom_html.window ## onload <- Dom_html.handler (fun _ -> *)
  (*     let imgs = Dom_html.document ## images in *)
  (*     for i = 0 to (imgs ## length) - 1 do *)
  (*       let image = imgs ## item (i) in *)
  (*       (Js.Optdef.iter image) (fun img ->  *)
  (*         img ## onclick <- Dom_html.handler  *)
  (*           (fun _ ->  *)
  (*             let src = img ## src in *)
  (*             let src = src ## replace_string ((Js.string "/thumbnails"), (Js.string "")) in *)
  (*             img ## src <- src; *)
  (*             Js._true)) *)
  (*     done; *)
  (*     Js._true) *)
(* let onload _ = *)
(*   let (>>=) = Js.Opt.bind in *)
(*   let canvas = Dom_html.createCanvas Dom_html.document in *)
(*   canvas ## setAttribute ((Js.string "style"), (Js.string "border-style:solid")); *)
(*   Dom.appendChild Dom_html.document ## body canvas; *)
(*   let ctx = canvas##getContext (Dom_html._2d_) in *)
(*   canvas##width <- width; canvas##height <- height; *)

  (* (get "canvas") >>= *)
  (*   (fun canvas -> *)

  (*     let canvas = ((Js.Unsafe.coerce canvas) : Dom_html.canvasElement Js.t) in *)
  (*     let ctx = canvas##getContext (Dom_html._2d_) in *)
  (*     ctx ## fillRect (10., 10., 20., 20.); *)
  (*     Js.Opt.return ()); *)
      (* Js.Opt.return ctx) >>= (fun canvas -> *)
      (*   int_value "balls" >>= (fun balls -> *)
      (*     int_value "red" >>= (fun red -> *)
      (*       int_value "blue" >>= (fun blue -> *)
      (*         int_value "radius" >>= (fun radius -> *)
      (*           float_value "speed" >>= (fun speed -> *)
      (*             let mouse = React_dom.mouse () in *)
      (*             let ticks,_ = React_dom.ticks 0.05 in *)
      (*             let phase = *)
      (*               S.l2 (fun ticks speed -> ticks *. speed *. 0.01) ticks speed in *)
      (*             let shapes = *)
      (*               S.l6 (fun balls radius red blue (mx, my) phase -> *)
      (*                 build_list balls (fun i -> *)
      (*                   let t = 2. *. 3.1415 *. float_of_int i /. float_of_int balls +. phase in *)
      (*                   let left, top = *)
      (*                     (float_of_int mx +. cos t *. radius, *)
      (*                      float_of_int my +. sin t *. radius) in *)
      (*                   let right, bottom = left +. 10., top +. 10.  *)
      (*                   in *)
      (*                   canvas ## fillRect (left, top, right, bottom))) *)
      (*             in *)
      (*             Js.Opt.return ())))))); *)
  (* Js._false *)

let _ = Dom_html.window ## onload <- Dom_html.handler Orbit.onload
(* let _ = *)
(*   let canvas = Dom_html.createCanvas Dom_html.document in *)
(*   Dom.appendChild Dom_html.document##body canvas *)
}}
