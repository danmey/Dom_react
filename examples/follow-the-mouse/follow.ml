module Html = Dom_html
module Dom = Dom
module Rd = React_dom
module R = Lwt_react
let js = Js.string

let iter f lst =
  for i = 0 to lst ## length - 1 do
    f (lst ## item (i))
  done

let onload ev =
  let (>>=) = Js.Opt.bind in

  (* let delay = 300. in *)

  let body = Html.document##getElementById (js"body") in
  
  (*
    node-valued behaviors are less interesting without auto-lifting;
    we could just make the divs once and set the position in an event
    handler, but this approximates the Flapjax code.
  *)
  let div ~id ~color ~backgroundColor ~position ~padding ~left ~top cs =
    let div = Html.createDiv Html.document in
    div ## setAttribute (js"id", js id);
    div ## style ## color <- js color;
    div ## style ## backgroundColor <- js backgroundColor;
    div ## style ## position <- js position;
    div ## style ## padding <- js padding;
    div ## style ## left <- js left;
    div ## style ## top <- js top;
    List.iter (fun c -> ignore(Dom.appendChild div (c :> Dom.node Js.t))) cs;
    div
  in

  let gen =
    let i = ref 0 in
    fun () -> incr i; !i
  in

  let mouse = Rd.mouse () in
  ignore(body >>= 
           (fun body ->
             ignore (Rd.appendChild body
               (R.S.l1 (fun (x, y) ->
                 let d = div
                   ~id:"themouse"
                   ~color:"#FFFFFF"
                   ~backgroundColor:"#ff0000"
                   ~position:"absolute"
                   ~left:(string_of_int x)
                   ~top:(string_of_int y)
                   ~padding:"10px" [ (* Html.createTextinput ~name:(js "the mouse!") Html.document *)] 
                 in
                (gen(), (d :> Dom.node Js.t)))  mouse));
             Html.document##getElementById (js"themouse") >>= fun themouse ->
             let mouse_offset = themouse ## offsetWidth in
             let tail_pos = 
               Rd.delay (R.S.l1 (fun (x, y) -> (float_of_int (x + mouse_offset), float_of_int y)) mouse) 200. in
             Rd.appendChild body
               (R.S.l1 (fun (x, y) ->
                 let d = div
                   ~id:"tail"
                   ~color:"#FF0000"
                   ~backgroundColor:"#ff0000"
                   ~position:"absolute"
                   ~left:(string_of_int (int_of_float x))
                   ~top:(string_of_int (int_of_float y))
                   ~padding:"10px" [ (* Html.createTextarea ~name:(js "the mouse!") Html.document *)] 
                 in
                (gen(), (d :> Dom.node Js.t))) tail_pos);
               Js.Opt.return ()));
  Js._false


(*   Fd.appendChild body *)
(*     (F.blift tail_pos (fun (x, y) -> *)
(*       div *)
(*         ~id:"tail" *)
(*         ~color:"#FF0000" *)
(*         ~backgroundColor:"#000000" *)
(*         ~position:"absolute" *)
(*         ~left:(string_of_int x) *)
(*         ~top:(string_of_int y) *)
(*         ~padding:"10px" *)
(*         [ D.document#createTextNode "its tail!" ])); *)


(*   let mouse_offset = (D.document#getElementById "themouse")#_get_offsetWidth in *)
(*   let tail_pos = F.blift (Fd.delay_b mouse delay) (fun (x, y) -> (x + mouse_offset, y)) in *)

(*   Fd.appendChild body *)
(*     (F.blift tail_pos (fun (x, y) -> *)
(*       div *)
(*         ~id:"tail" *)
(*         ~color:"#FF0000" *)
(*         ~backgroundColor:"#000000" *)
(*         ~position:"absolute" *)
(*         ~left:(string_of_int x) *)
(*         ~top:(string_of_int y) *)
(*         ~padding:"10px" *)
(*         [ D.document#createTextNode "its tail!" ])); *)

(*   let wag_delay = delay *. 1.5 in *)
(*   let mouseandtail_offset = mouse_offset + (D.document#getElementById "tail")#_get_offsetWidth in *)
(*   let wag_offset = F.hold 0 (F.collect (fun _ _ -> (Random.int 10) - 5) 0 (Fd.ticks 100.)) in *)
(*   let wag_pos = *)
(*     F.blift2 *)
(*       (Fd.delay_b mouse wag_delay) wag_offset *)
(*       (fun (x, y) wag_offset -> *)
(*         (x + mouseandtail_offset, y + wag_offset)) in *)

(*   Fd.appendChild body *)
(*     (F.blift wag_pos (fun (x, y) -> *)
(*       div *)
(*         ~id:"wagging" *)
(*         ~color:"#FFFF00" *)
(*         ~backgroundColor:"#000000" *)
(*         ~position:"absolute" *)
(*         ~left:(string_of_int x) *)
(*         ~top:(string_of_int y) *)
(*         ~padding:"10px" *)
(*         [ D.document#createTextNode "is happy!" ])); *)

(* ;; *)
;;
Html.window##onload <- (Html.handler onload)
