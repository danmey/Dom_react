(* Converted from Jake Donham OCamljs Froc example *)

open Lwt_react

let build_list n f =
  if n < 0 then invalid_arg "n";
  let rec bl k =
    if k = n then []
    else f k :: bl (k + 1) in
  bl 0

let onload _ =
  let (>>=) = Js.Opt.bind in
  (Dom_html.document ## getElementById (Js.string "canvas")) >>=
    (fun canvas ->
      let canvas = ((Js.Unsafe.coerce canvas) : Dom_html.canvasElement Js.t) in
      let ctx = canvas  ##getContext (Dom_html._2d_) in
      React_dom.int_value "balls" >>= (fun balls ->
      React_dom.int_value "red" >>= (fun red ->
      React_dom.int_value "blue" >>= (fun blue ->
      React_dom.float_value "radius" >>= (fun radius ->
      React_dom.float_value "speed" >>= (fun speed ->
        let mouse = React_dom.mouse () in
        let ticks,_ = React_dom.ticks 0.05 in
        let phase =
          S.l2 (fun ticks speed -> ticks *. speed *. 0.01) ticks speed in
        let shapes =
          S.l6 (fun balls radius red blue (left, top) phase ->
            ctx ## clearRect (0., 0., 
                              float_of_int (canvas ## clientWidth), 
                              float_of_int (canvas ## clientHeight));
            build_list balls 
              (fun i ->
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

