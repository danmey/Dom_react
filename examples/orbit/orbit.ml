
open Lwt_react

let get id = Dom_html.document ## getElementById (Js.string id)

let mk_input_value f =
  let element = Dom_html.createInput (Dom_html.document) in
  Dom.appendChild (Dom_html.document ## body) element;
  S.map f (React_dom.input_value "4" element)

(* let input_value f id = *)
(*   Js.Opt.map (get id) *)
(*     (fun element -> *)
(*       let element = ((Js.Unsafe.coerce element) : Dom_html.inputElement Js.t) in *)
(*       S.map f (React_dom.input_value "1111" element)) *)

let input_value f name =
  mk_input_value f

let int_value = input_value int_of_string
let float_value = input_value float_of_string

let build_list n f =
  if n < 0 then invalid_arg "n";
  let rec bl k =
    if k = n then []
    else f k :: bl (k + 1) in
  bl 0

open React_dom

let onload _ =
  let (>>=) = Js.Opt.bind in
  let canvas = Dom_html.createCanvas Dom_html.document in
  canvas ## setAttribute ((Js.string "style"), (Js.string "border-style:solid"));
  Dom.appendChild Dom_html.document ## body canvas;
  let ctx = canvas##getContext (Dom_html._2d_) in
  ctx ## fillRect (10., 10., 20., 20.);
  (* (get "canvas") >>= *)
  (*   (fun element -> *)
  (*     Dom_html.window ## alert (Js.string "nice2"); *)
  (*   (\* let ctx = ((Js.Unsafe.coerce element) : Dom_html.canvasElement Js.t)  ##getContext (Dom_html._2d_) in *\) *)
  (*     (\* ctx ## fillRect (10., 10., 20., 20.); *\) *)
  (*   Js.Opt.return ()); *)
      
  (*     let canvas = ((Js.Unsafe.coerce canvas) : Dom_html.canvasElement Js.t) in *)
  (* int_value "balls" >>= (fun balls -> *)
  (*   int_value "red" >>= (fun red -> *)
  (*     int_value "blue" >>= (fun blue -> *)
  (*       int_value "radius" >>= (fun radius -> *)
  (*         float_value "speed" >>= (fun speed -> *)
  (*           let _ = 0 in *)
  (*             ctx ## fillRect (10., 10., 20., 20.); *)
  (*           (\* let mouse = React_dom.mouse () in *\) *)
  (*           (\* let ticks,_ = React_dom.ticks 0.05 in *\) *)
  (*           (\* let phase = *\) *)
  (*           (\*   S.l2 (fun ticks speed -> ticks *. speed *. 0.01) ticks speed in *\) *)
  (*           (\* let shapes = *\) *)
  (*           (\*   S.l6 (fun balls radius red blue (mx, my) phase -> *\) *)
  (*           (\*     build_list balls (fun i -> *\) *)
  (*           (\*       let t = 2. *. 3.1415 *. float_of_int i /. float_of_int balls +. phase in *\) *)
  (*           (\*       let left, top = *\) *)
  (*           (\*         (float_of_int mx +. cos t *. radius, *\) *)
  (*           (\*          float_of_int my +. sin t *. radius) in *\) *)
  (*           (\*       let right, bottom = left +. 10., top +. 10. *\) *)
  (*           (\*       in *\) *)
  (*           (\*       ctx ## fillRect (left, top, right, bottom))) *\) *)
  (*           Js.Opt.return ()))))); *)
  let balls = int_value "balls" in
  let red = int_value "red" in
  let blue = int_value "blue" in
  let radius = float_value "radius" in
  let speed = float_value "speed" in
  let mouse = React_dom.mouse () in
  let ticks,_ = React_dom.ticks 0.05 in
  let phase =
    S.l2 (fun ticks speed -> ticks *. speed *. 0.01) ticks speed in
  let shapes =
    S.l6 (fun balls radius red blue (left, top) phase ->
        ctx ## clearRect (0., 0., float_of_int (canvas ## clientWidth), float_of_int (canvas ## clientHeight));

      build_list balls (fun i ->
        let t = 2. *. 3.1415 *. float_of_int i /. float_of_int balls +. phase in
        let left, top = float_of_int left, float_of_int top in
        let left, top =
          (left +. cos t *. 10.*.radius),
           (top +. sin t *. 10.*.radius) in
        let x0, y0 = Dom_html.elementClientPosition canvas in
        let left, top = left -. 5. -. float_of_int x0, top -. 5. -. float_of_int y0 in
        ctx ## fillRect (left, top, 10., 10.))) balls radius red blue mouse phase in


  Js._false

(* let _ = Dom_html.window ## onload <- Dom_html.handler onload *)

