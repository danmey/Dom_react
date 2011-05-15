(* Converted from Jake Donham OCamljs Froc example *)

open Lwt_react

module React_dom = struct
open Dom_html

let ticks ms =
  let signal, send_time = S.create 0. in
  let time = ref 0. in
  let rec timeout_id = ref (lazy (window ## setTimeout (Js.wrap_callback loop, ms)))
  and loop ev =
    send_time !time;
    time := !time +. ms;
    timeout_id := (lazy window ## setTimeout (Js.wrap_callback loop, ms));
    ignore (Lazy.force !timeout_id);
  in
  ignore (Lazy.force !timeout_id);
  signal, (fun () -> window ## clearTimeout (Lazy.force !timeout_id))

let input_value value (element : inputElement Js.t) =
  let event, send_event = S.create value in
  element ## onchange <-
    (handler (fun ev -> send_event (Js.to_string (element ## value)); Js._false));
  event

let mouse () =
  let event, send_event = S.create (0,0) in
  let receive ev =
    let mx, my = eventAbsolutePosition ev in
    send_event (mx, my);
    Js._false
  in
  addEventListener (document :> eventTarget Js.t) Event.mousemove (handler receive) Js._false;
  event
end

let get id = Dom_html.document ## getElementById (Js.string id)

let mk_input_value f =
  let element = Dom_html.createInput (Dom_html.document) in
  Dom.appendChild (Dom_html.document ## body) element;
  S.map f (React_dom.input_value "4" element)

let input_value f id =
  Js.Opt.map (get id)
    (fun element ->
      let element = ((Js.Unsafe.coerce element) : Dom_html.inputElement Js.t) in
      S.map f (React_dom.input_value "3" element))

(* let input_value f name = *)
(*   mk_input_value f *)

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
  (get "canvas") >>=
    (fun canvas ->
      let canvas = ((Js.Unsafe.coerce canvas) : Dom_html.canvasElement Js.t) in
      let ctx = canvas  ##getContext (Dom_html._2d_) in
      ctx ## fillRect (10., 10., 20., 20.);
      int_value "balls" >>= (fun balls ->
        int_value "red" >>= (fun red ->
          int_value "blue" >>= (fun blue ->
            float_value "radius" >>= (fun radius ->
              float_value "speed" >>= (fun speed ->
                let mouse = React_dom.mouse () in
                let ticks,_ = React_dom.ticks 0.05 in
                let phase =
                  S.l2 (fun ticks speed -> ticks *. speed *. 0.01) ticks speed in
                let shapes =
                  S.l6 (fun balls radius red blue (left, top) phase ->
                    ctx ## clearRect (0., 0., 
                                      float_of_int (canvas ## clientWidth), 
                                      float_of_int (canvas ## clientHeight));
                    build_list balls (fun i ->
                      let t = 2. *. 3.1415 *. float_of_int i /. float_of_int balls +. phase in
                      let left, top = float_of_int left, float_of_int top in
                      let left, top =
                        (left +. cos t *. 10.*.radius),
                        (top +. sin t *. 10.*.radius) in
                      let x0, y0 = Dom_html.elementClientPosition canvas in
                      let left, top = left -. 5. -. float_of_int x0, top -. 5. -. float_of_int y0 in
                      ctx ## fillRect (left, top, 10., 10.))) balls radius red blue mouse phase in
                Js.Opt.return ()))))));
  Js._false
    
let _ = Dom_html.window ## onload <- Dom_html.handler onload 

