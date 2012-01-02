module S = Base.S
module H = Dom_html

module Value = struct
 
  (* Due ot limitation of type inference ( we need to wrap conversion
  functions to a first class module *)

  exception WrongFormat
  module type CONVERSION = sig
    type t
    val default : string
    val string_of : t -> string
    val of_string : string -> t
  end
    
  module IntConversion = struct
    type t = int
    let default = "0"
    let string_of = string_of_int
    let of_string = int_of_string
  end

  module FloatConversion = struct
    type t = float
    let default = "0."

    let string_of f =
      let s = string_of_float f in 
      if s = "nan" then 
        raise WrongFormat 
      else s

    let of_string s = 
      try float_of_string s 
      with _ -> float (int_of_string s)
  end
      
  module StringConversion = struct
    type t = string
    let default = ""
    let string_of s = s
    let of_string s = s
  end

  let numerical (type t) conversion value =
    let module C = (val conversion : CONVERSION with type t = t) in
    let w = H.createInput ~_type:(Js.string "text") Dom_html.document in
    w ## value <- Js.string (C.string_of value);
    let e = S.create w S.onchar in
    let validate = S.map
      (fun char_code ->
            let input = Js.to_string w ## value in
            try
              let selectionStart = w ## selectionStart in
              let selectionEnd = w ## selectionEnd in
              let left,right =
                if input = "" then
                  "","" else
                  String.sub input 0 selectionStart, 
                  String.sub input selectionStart 
                    (String.length input - selectionStart) 
              in
              let input' = Printf.sprintf "%s%c%s" left (Char.chr char_code) right in
              let input' = C.string_of (C.of_string input') in
              let input'' = C.string_of (C.of_string input') in
              if input' = input'' then begin
                w ## value <- Js.string input';
                w ## selectionStart <- selectionStart+1;
                w ## selectionEnd <- selectionStart+1;
                input'
              end
              else input
          with _ -> input
      ) e 
    in
    w, S.map (fun value -> try C.of_string value with _ -> C.of_string C.default) validate
      
  let int = numerical (module IntConversion : CONVERSION with type t = int)
  let float = numerical (module FloatConversion : CONVERSION with type t = float)
  let string = numerical (module StringConversion : CONVERSION with type t = string)
  (* let float value = numerical float_of_int int_of_float value *)
end
