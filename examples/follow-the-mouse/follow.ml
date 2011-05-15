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

  let delay = 300. in

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
  ignore(Html.document##getElementById (js"body") >>= 
           (fun body ->
             ignore (Rd.appendChild body
               (R.S.l1 (fun (x, y) ->
                 let d = div
                   ~id:"themouse"
                   ~color:"#FFFFFF"
                   ~backgroundColor:"#000000"
                   ~position:"absolute"
                   ~left:(string_of_int x)
                   ~top:(string_of_int y)
                   ~padding:"10px" [ Html.document ## createTextNode (js"the mouse!") ] 
                 in
                (gen(), (d :> Dom.node Js.t)))  mouse));

             Html.document ## getElementById (js"themouse") >>= fun themouse ->
             let mouse_offset = themouse ## offsetWidth in
             let tail_pos = 
               Rd.delay (R.S.l1 (fun (x, y) -> (x + mouse_offset), y) mouse) delay in
             Rd.appendChild body
               (R.S.l1 (fun (x, y) ->
                 let d = div
                   ~id:"tail"
                   ~color:"#FF0000"
                   ~backgroundColor:"#000000"
                   ~position:"absolute"
                   ~left:(string_of_int x)
                   ~top:(string_of_int y)
                   ~padding:"10px" [ Html.document ## createTextNode (js"its tail!") ] 
                 in
                (gen(), (d :> Dom.node Js.t))) tail_pos);

           let wag_delay = delay *. 1.5 in
           Html.document ## getElementById (js"tail") >>= fun tail ->
           let mouseandtail_offset = mouse_offset + tail ## offsetWidth in
           let ticks, _ = Rd.ticks 100. in
           let wag_offset = R.S.map (fun _ -> Random.int 10 - 5) ticks in
           let wag_pos = R.S.l2
             (fun (x, y) wag_offset -> (x + mouseandtail_offset, y + wag_offset)) 
             (Rd.delay mouse wag_delay) wag_offset in
             Rd.appendChild body
               (R.S.l1 (fun (x, y) ->
                 let d = div
                   ~id:"tail"
                   ~color:"#FFFF00"
                   ~backgroundColor:"#000000"
                   ~position:"absolute"
                   ~left:(string_of_int x)
                   ~top:(string_of_int y)
                   ~padding:"10px" [ Html.document ## createTextNode (js"is happy!") ] 
                 in
                (gen(), (d :> Dom.node Js.t))) wag_pos);
           Js.Opt.return ()));
  Js._false

(* ;; *)
;;
Html.window##onload <- (Html.handler onload)
