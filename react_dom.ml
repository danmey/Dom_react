open Lwt_react
open Dom_html

let input_value value (element : inputElement Js.t) =
  let event, send_event = S.create value in
  element ## value <- Js.string value;
  element ## onchange <-
    (handler (fun ev -> send_event (Js.to_string (element ## value)); Js._false));
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
    old := Some r;
    n
  in
  S.l1 update nb

let replaceChild old nb =
  let old = (old :> Dom.node Js.t) in
  let old = ref old in
  let update (_,newel) =
    Js.Opt.map (!old ## parentNode) (fun parent -> parent ## replaceChild (newel, !old);
    old := newel)
  in
  S.l1 update nb

let delay ms s =
  let accuracy = 10. in
  let time = ref 0. in
  let pending = Queue.create () in
  let news, send = S.create None in
  let send _ =
    time := !time +. 10.;
    let rec loop () =
      try
        let t,(_,s) = Queue.peek pending in
        if t < !time -. ms then
          begin
            let (_,s) = Queue.pop pending in
            send (Some s);
            loop ()
          end
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
    ignore (S.l1 (fun x -> Queue.add (!time, x) pending) s);
    S.fmap (fun x -> x) (S.value s) news
    

module S = struct
  module Html = Dom_html
  let js = Js.string
  let ticks ms =
    let ms' = float ms in
    let signal, send_time = S.create 0 in
    let time = ref 0 in
    let rec timeout_id = ref (lazy (window ## setTimeout (Js.wrap_callback loop, ms')))
    and loop ev =
      window ## clearTimeout (Lazy.force !timeout_id);
      send_time !time;
      time := !time + ms;
      timeout_id := (lazy window ## setTimeout (Js.wrap_callback loop, ms'));
      ignore (Lazy.force !timeout_id);
    in
    ignore (Lazy.force !timeout_id);
    signal

  let time () = React.S.map (fun t -> float_of_int t /. 1000.0) (ticks 20)
    
  let mouse () =
    let event, send_event = S.create (0,0) in
    let receive ev =
      let mx, my = eventAbsolutePosition ev in
      send_event (mx, my);
      Js._false
    in
    ignore (addEventListener (document :> eventTarget Js.t) Event.mousemove (handler receive) Js._false);
  event

  let mousef () = S.map (fun (x,y) -> float_of_int x, float_of_int y) (mouse ())

  let div ~id ~color ~backgroundColor ~position ~padding ~left ~top parent =
    let div = Html.createDiv Html.document in
    div ## setAttribute (js"id", js id);
    div ## style ## color <- js color;
    div ## style ## backgroundColor <- js backgroundColor;
    div ## style ## position <- js position;
    div ## style ## padding <- js padding;
    div ## style ## left <- js left;
    div ## style ## top <- js top;
    Dom.appendChild parent div;
    S.const div

  module Css = struct 
    type selector =
      [
      |`background
      |`backgroundAttachment
      |`backgroundColor
      |`backgroundImage
      |`backgroundPosition
      |`backgroundRepeat
      |`border
      |`borderBottom
      |`borderBottomColor
      |`borderBottomStyle
      |`borderBottomWidth
      |`borderCollapse
      |`borderColor
      |`borderLeft
      |`borderLeftColor
      |`borderLeftStyle
      |`borderLeftWidth
      |`borderRight
      |`borderRightColor
      |`borderRightStyle
      |`borderRightWidth
      |`borderSpacing
      |`borderStyle
      |`borderTop
      |`borderTopColor
      |`borderTopStyle
      |`borderTopWidth
      |`borderWidth
      |`bottom
      |`captionSide
      |`clear
      |`clip
      |`color
      |`content
      |`counterIncrement
      |`counterReset
      |`cssText
      |`cursor
      |`direction
      |`display
      |`emptyCells
      |`font
      |`fontFamily
      |`fontSize
      |`fontStyle
      |`fontVariant
      |`fontWeight
      |`height
      |`left
      |`letterSpacing
      |`lineHeight
      |`listStyle
      |`listStyleImage
      |`listStylePosition
      |`listStyleType
      |`margin
      |`marginBottom
      |`marginLeft
      |`marginRight
      |`marginTop
      |`maxHeight
      |`maxWidth
      |`minHeight
      |`minWidth
      |`opacity
      |`outline
      |`outlineColor
      |`outlineOffset
      |`outlineStyle
      |`outlineWidth
      |`overflow
      |`overflowX
      |`overflowY
      |`padding
      |`paddingBottom
      |`paddingLeft
      |`paddingRight
      |`paddingTop
      |`pageBreakAfter
      |`pageBreakBefore
      |`position
      |`right
      |`tableLayout
      |`textAlign
      |`textDecoration
      |`textIndent
      |`textTransform
      |`top
      |`verticalAlign
      |`visibility
      |`whiteSpace
      |`width
      |`wordSpacing
      |`zIndex ]
        
    let change el v = function
      | `left -> el ## style ## left <- v
      | `top -> el ## style## top <- v

    let px i = js (Printf.sprintf "%d" (int_of_float i))
    let percent f =
      let p = int_of_float (f *. 100.) in
      js (Printf.sprintf "%d%%" p)
    let id i = js (string_of_int i)
      
    let style ~sel ~typ = S.l2 (fun el s -> change el (typ s) sel)
    let ccolor (r,g,b) = js (Printf.sprintf "#000000")
    let c = React.S.const
  let element ?parent:(parent=Html.document ## body) 
      ?position
      ?color:(color=c (0.,0.,0.))
      ?width:(width=c 100.)
      ?height:(height=c 100.)
      ?left:(left = c 0.)
      ?top:(top = c 0.) () =

    let div = Html.createDiv Html.document in
    Dom.appendChild parent div;
    let position = match position with
      | Some p -> p
      | None -> S.l2 (fun left top -> left, top) left top in

    S.l4 (fun (x,y) color width height ->
      div ## style ## backgroundColor <- ccolor color;
      div ## style ## position <- js"absolute";
      div ## style ## left <- px x;
      div ## style ## top <- px y;
      div ## style ## width <- px width;
      div ## style ## height <- px height;
      div ## style ## backgroundColor <- ccolor color;
      div) position color width height

  let get att el =
    match att with
      | `left -> S.l1 (fun el -> float_of_string (Js.to_string (el ## style ## left))) el
      | `top -> S.l1 (fun el -> float_of_string (Js.to_string (el ## style ## top))) el

  let set el att v =
    ignore (match att with
      | `left -> S.l2 (fun el v -> el ## style ## left <- js (string_of_int (int_of_float v))) el v
      | `top -> S.l2 (fun el v -> el ## style ## top <- js (string_of_int (int_of_float v))) el v);
        el

  (* let position el =  *)
  (*       S.l1 (fun el -> float_of_string (Js.to_string (el ## style ## left))) el, *)
  (*       S.l1 (fun el -> float_of_string (Js.to_string (el ## style ## top))) el *)
      

  let (>>) x f = f x
  let (>>=) v g = g v
  end

end

module E = struct

end
