module E = Dom_react.E
module H = Dom_html

module Value = struct
 
  let int value =
    let w = H.createInput ~_type:(Js.string "text") Dom_html.document in
    w ## value <- Js.string (string_of_int value);
    let e = E.create w E.onchar in
    let validate = E.map
      (fun char_code ->
          let input = Js.to_string w ## value in
          let selectionStart = w ## selectionStart in
          let selectionEnd = w ## selectionEnd in
          let left = String.sub input 0 selectionStart in
          let right = String.sub input selectionStart 
            (String.length input - selectionStart) in
          try
            let input = Printf.sprintf "%s%c%s" left (Char.chr char_code) right in
            int_of_string input;
            w ## value <- Js.string input;
            w ## selectionStart <- selectionStart+1;
            w ## selectionEnd <- selectionStart+1;
            input
          with _ -> input
      ) e 
    in
    w, E.map int_of_string validate

end
