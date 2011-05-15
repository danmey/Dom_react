open Lwt_react
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
  element ## value <- Js.string value;
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
  ignore (addEventListener (document :> eventTarget Js.t) Event.mousemove (handler receive) Js._false);
  event

let get id = Dom_html.document ## getElementById (Js.string id)

let create_input_value f =
  let element = Dom_html.createInput (Dom_html.document) in
  Dom.appendChild (Dom_html.document ## body) element;
  S.map f (input_value "4" element)

let input_value f id =
  Js.Opt.map (get id)
    (fun element ->
      let element = ((Js.Unsafe.coerce element) : Dom_html.inputElement Js.t) in
      S.map f (input_value "3" element))

(* let input_value f name = *)
(*   mk_input_value f *)

let int_value = input_value int_of_string
let float_value = input_value float_of_string
  
let appendChild n nb =
  let n = (n :> Dom.node Js.t) in
  let old = ref None in
  let update (_,r) =
  ignore
    (match !old with
      | None -> Dom.appendChild n r
      | Some oc -> Dom.replaceChild n r oc);
    old := Some r
  in
  S.l1 update nb

let delay s =
  let accuracy = 10. in
  let time = ref 0. in
  fun ms ->
  let pending = Queue.create () in
  let news, send = S.create None in
  let send _ =
    time := !time +. 10.;
    let rec loop () =
      try
      (match Queue.peek pending with
          (t, s) ->
            if t < !time -. ms then
              begin
                let (_,s) = Queue.pop pending in
                send (Some s);
                loop ()
              end)
      with Queue.Empty -> ()
    in
      loop ()
  in
  let rec timeout_id = ref (lazy (window ## setTimeout (Js.wrap_callback (loop send), accuracy)))
  and loop f ev =
    f ();
    timeout_id := (lazy window ## setTimeout (Js.wrap_callback (loop send), accuracy));
    ignore (Lazy.force !timeout_id);
  in
    ignore (Lazy.force !timeout_id);
    S.l1 (fun x -> Queue.add (!time, x) pending) s;
    S.fmap (fun x -> x) (S.value s) news
    

  
  
    

    
  
