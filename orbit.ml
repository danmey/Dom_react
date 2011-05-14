open Lwt_react
let get id = Dom_html.document ## getElementById (Js.string id)

let input_value f id =
  Js.Opt.map (get id) 
    (fun element ->
      let element = ((Js.Unsafe.coerce element) : Dom_html.inputElement Js.t) in
      S.map f (React_dom.input_value element))

let input_value f id =
  Js.Opt.map (get id) 
    (fun element ->
      let element = ((Js.Unsafe.coerce element) : Dom_html.inputElement Js.t) in
      S.map f (React_dom.input_value element))

let int_value = input_value int_of_string
let float_value = input_value float_of_string

let build_list n f =
  if n < 0 then invalid_arg "n";
  let rec bl k =
    if k = n then []
    else f k :: bl (k + 1) in
  bl 0

let onload _ =
  let (>>=) = Js.Opt.bind in
  (get "canvas") >>=
    (fun canvas ->
      let canvas = ((Js.Unsafe.coerce canvas) : Dom_html.canvasElement Js.t) in
      let ctx = canvas##getContext (Dom_html._2d_) in
      ctx ## fillRect (10., 10., 20., 20.);
      Js.Opt.return ());
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
  Js._false

let _ = Dom_html.window ## onload <- Dom_html.handler onload
