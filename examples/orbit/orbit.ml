(* Converted from Jake Donham OCamljs Froc example *)

module Input = Dom_react.Input
module S = React.S

let build_list n f =
  if n < 0 then invalid_arg "n";
  let rec bl k =
    if k = n then []
    else f k :: bl (k + 1) in
  bl 0

let onload _ =
  let (>>=) = Js.Opt.bind in
  let _ = Dom_html.document##getElementById (Js.string"body") >>=
    (fun body ->
      (Dom_html.document ## getElementById (Js.string "canvas")) >>=
        (fun canvas ->
          let canvas = ((Js.Unsafe.coerce canvas) : Dom_html.canvasElement Js.t) in
          let ctx = canvas ## getContext (Dom_html._2d_) in
          let _, balls = Input.Named.int 5 "balls" in
          let _, red = Input.Named.int 1 "red" in
          let _, green = Input.Named.int 1 "green" in
          let _, blue = Input.Named.int 2 "blue"in
          let _, radius = Input.Named.float 0.5 "radius" in
          let _, speed = Input.Named.float 0.6 "speed" in

          let ticks = Dom_react.Base.OldS.time () in
          let phase =
            S.l2 (fun ticks speed -> ticks *. speed *. 0.01) ticks speed in

          let shapes =
            S.l5 (fun balls radius red blue phase ->
              let left, top = 100, 100 in
              ctx ## clearRect (0., 0.,
                                float_of_int (canvas ## clientWidth),
                                float_of_int (canvas ## clientHeight));
              let _ = build_list balls
                (fun i ->
                  let t = 2. *. 3.1415 *. float_of_int i /. float_of_int balls +. phase in
                  let left, top = float_of_int left, float_of_int top in
                  let left, top =
                    (left +. cos t *. 10.*.radius),
                    (top +. sin t *. 10.*.radius) in
                  let x0, y0 = Dom_html.elementClientPosition canvas in
                  let left, top = left -. 5. -. float_of_int x0, top -. 5. -. float_of_int y0 in
                  ctx ## fillRect (left, top, 10., 10.)) in
            ()) balls radius red blue phase
          in
          Js.Opt.return ())) in
      Js._false
    
let _ = Dom_html.window ## onload <- Dom_html.handler onload 

