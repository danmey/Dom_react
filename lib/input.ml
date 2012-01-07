module S = Base.S
module E = Base.E
module H = Dom_html

  (* Due to limitation of type inference we need to wrap conversion
     functions to a first class module *)
module type CONVERSION = sig
  type t
  type widget
  val default : string
  val set_string : widget -> string -> unit
  val set : widget -> t -> unit
  val get : widget -> t
  val string_of : t -> string
  val of_string : string -> t
end

exception Wrong_format
  
module ValueControl(T : sig 
  type t
  type widget
  val default : string
  val string_of : t -> string
  val of_string : string -> t
  end) = struct
  include T
  let set_string w str = w ## value <- Js.string str
  let set w v = set_string w (T.string_of v)
  let get w = T.of_string (Js.to_string (w ## value))
end

module IntConversion = struct
  module C = struct
    type t = int
    let default = "0"
    let string_of = string_of_int
    let of_string = int_of_string
  end
  include ValueControl(struct type widget = H.inputElement Js.t include C end)
end

module FloatConversion = struct
  module C = struct
    type t = float
    let default = "0."

    let string_of f =
      let s = string_of_float f in 
      if s = "nan" then 
        raise Wrong_format 
      else s

    let of_string s = 
      try float_of_string s 
      with _ -> float (int_of_string s)
  end
  include ValueControl(struct type widget = H.inputElement Js.t include C end)
end
  
module StringConversion = struct
  module C = struct
    type t = string
    let default = ""
    let string_of s = s
    let of_string s = s
  end
  include ValueControl(struct type widget = H.inputElement Js.t include C end)
end

module BoolConversion = struct
  type t = bool
  type widget = H.inputElement Js.t
  let default = "off"
  let string_of s = if s then "on" else "off"
  let of_string = function
    | "on" -> true
    | "off" -> false
    | _ -> raise Wrong_format
  let set_string w str = w ## value <- Js.string str
  let set w v = set_string w (string_of v)
  let get w = of_string (Js.to_string (w ## value))
end

module Create = struct

  let custom (type t) type_ creator conversion value =
    let module C = (val conversion : CONVERSION 
                    with 
                      type t = t 
                    and type widget = H.inputElement Js.t) in
    let w = H.createInput ~_type:(Js.string type_) Dom_html.document in
    (* C.set w (Js.string (C.string_of value)); *)
    let e = creator w in
    let validate = S.map
      (fun char_code ->
        let input = C.get w in
        let input = C.string_of input in
        try
          let selectionStart = w ## selectionStart in
          let handle_del incr =
            let left_i, right_i = incr selectionStart in
            let input, selectionStart =
              let left,right =
                String.sub input 0 left_i,
                String.sub input right_i 
                  (String.length input - right_i) 
              in
              let input = left ^ right in
              input, selectionStart
            in
            C.set_string w input;
            w ## selectionStart <- left_i;
            w ## selectionEnd <- left_i;
            input
          in
          match char_code with
            | 46 -> handle_del (fun n -> n,n+1)
            | 8 -> handle_del (fun n -> n - 1, n)
            | char_code ->
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
                C.set_string w input';
                w ## selectionStart <- selectionStart+1;
                w ## selectionEnd <- selectionStart+1;
                input'
              end
              else input
        with _ -> input
      ) e 
    in
    w, S.map (fun value -> try C.of_string value with _ -> C.of_string C.default) validate

  let bool name =
    let w = H.createInput ~_type:(Js.string "checkbox") Dom_html.document in
    let on_click = S.create w S.onclick in
    w, S.map (Base.Fun_prop.checked w) on_click

  let on_char w = S.create w S.onchar

  let int v = custom "text" on_char
    (module IntConversion : CONVERSION 
      with type t = int 
      and type widget = H.inputElement Js.t) v

  let float v = custom "text" on_char
    (module FloatConversion : CONVERSION 
      with type t = float 
      and type widget = H.inputElement Js.t) v

  let string v = custom "text" on_char
    (module StringConversion : CONVERSION 
      with type t = string 
      and type widget = H.inputElement Js.t) v

  let button name =
    let w = Dom_html.createButton Dom_html.document in
    let e = E.create w E.onclick in
    w ## innerHTML <- Js.string name;
    w, e
end
  
module Map = struct
  let custom (type t) type_ conversion s =
    let module C = (val conversion : CONVERSION with type t = t) in
    let w = H.createInput ~_type:(Js.string type_) Dom_html.document in
    w, S.map (fun n -> (Base.Fun_prop.set_value w (C.string_of n))) s 

  let int = custom "text" (module IntConversion : CONVERSION with type t = int)
  let float = custom "text" (module FloatConversion : CONVERSION with type t = float)
  let string = custom "text" (module StringConversion : CONVERSION with type t = string)
  let bool s =
    let w = H.createInput ~_type:(Js.string "checkbox") Dom_html.document in
    w, S.map (Base.Fun_prop.set_checked w) s
end
