open Lwt_react
open Dom_html

  
let ticks ms =
  let signal, send_time = S.create 0. in
  let time = ref 0. in
  let rec timeout_id = ref (lazy (window ## setTimeout (Js.wrap_callback loop, ms)))
  and loop ev =
    send_time !time;
    time := !time +. ms;
    timeout_id := (lazy window ## setTimeout (Js.wrap_callback loop, ms))
  in
  ignore (Lazy.force !timeout_id);
  signal, (fun () -> window ## clearTimeout (Lazy.force !timeout_id))


let input_value (element : inputElement Js.t) =
  let event, send_event = S.create "" in
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
  

