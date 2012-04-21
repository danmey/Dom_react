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
  let element id = Js.Opt.get 
    (Dom_html.document##getElementById (Js.string id))
    (fun () -> failwith "no element id") 
  in

  let body = element "body" in
  let canvas = element "canvas" in

  let canvas = ((Js.Unsafe.coerce canvas) : Dom_html.canvasElement Js.t) in
  let ctx = canvas ## getContext (Dom_html._2d_) in

  let _, radius = Input.Named.float 5. "radius" in
  let _, speed = Input.Named.float 6. "speed" in
  let _, balls = Input.Named.int 10 "balls" in
  let _, red = Input.Named.int 0 "red" in
  let _, green = Input.Named.int 0 "green" in
  let _, blue = Input.Named.int 0 "blue"in
  
  let ticks = Dom_react.Time.time () in
  let phase =
    S.l2 (fun ticks speed -> ticks *. speed *. 0.01) ticks speed 
  in

  let draw balls radius red green blue phase =
    let left, top = 100, 100 in
    
    ctx ## clearRect (0., 0.,
                      float_of_int (canvas ## clientWidth),
                      float_of_int (canvas ## clientHeight));

    ctx ## fillStyle <- Js.string (Printf.sprintf "rgb(%d,%d,%d)" red green blue);

    let _ = build_list balls
      (fun i ->
        let t = 2. *. 3.1415 *. float_of_int i /. float_of_int balls +. phase in
        let left, top = float_of_int left, float_of_int top in
        let left, top =
          (left +. cos t *. 10.*.radius),
          (top +. sin t *. 10.*.radius) in
        let x0, y0 = Dom_html.elementClientPosition canvas in
        let left, top = left -. 5. -. float_of_int x0, top -. 5. -. float_of_int y0 
        in
        ctx ## fillRect (left, top, 10., 10.)) 
    in
    ()
  in
  S.l6 draw balls radius red green blue phase;
  Js._false
    
let _ = Dom_html.window ## onload <- Dom_html.handler onload 
