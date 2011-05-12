module Html = Dom_html
open Lwt
let js = Js.string

let document = Html.document
let chess_board_size = 8
let draw_board (crown_white, crown_black) board_div buf =
  let m = Html.createTable document in
  m##cellSpacing <- js"0";
  m##cellPadding <- js"0";
  let rec board = lazy (Array.init chess_board_size (fun y ->
    let tr = m##insertRow (-1) in
    Array.init chess_board_size (fun x ->
      let td = tr##insertCell (-1) in
      let img = Html.createImg document in
      let div = Html.createDiv document in

      Dom.appendChild tr td;
      Dom.appendChild td div;
      Dom.appendChild div img;

      div##style##width <- js"54px";
      div##style##height <- js"54px";
      div##style##textAlign <- js "center";
      div##style##verticalAlign <- js "middle";
      div##style##minHeight <- js"10em";
      div##style##display <- js"table-cell";
      img##src <- js"bishop-bb.png";
    ))) in
  (* Reverse the board ranks *)
      let board = Lazy.force board in
  Dom.appendChild buf m;
  ()

let init_table board_div =
  let buf = document##createDocumentFragment() in
  let img1 = Html.createImg document in
  let img2 = Html.createImg document in
  let div1 = Html.createDiv document in
  let div2 = Html.createDiv document in
  img1##src <- js"sprites/crown-nb.png";
  div1##style##textAlign <- js "center";
  img2##src <- js"sprites/crown-fb.png";
  div2##style##textAlign <- js "center";
  board_div##style##width <- js"432px";
  board_div##style##lineHeight <- js"0";
  img1##style##visibility <- js"hidden";
  draw_board (img2, img1) board_div buf;
  Dom.appendChild div1 img1;
  Dom.appendChild div2 img2;
  Dom.appendChild board_div div1;
  Dom.appendChild board_div buf;
  Dom.appendChild board_div div2

let run div =
  init_table div


let onload _ =
  let main =
    Js.Opt.get (document##getElementById(js"board"))
      (fun () -> assert false)
  in
  let div = Html.createDiv document in
  Dom.appendChild main div;
  run div;
  Js._false

let _ = Html.window##onload <- Html.handler onload

