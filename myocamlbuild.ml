open Ocamlbuild_plugin;;

let run_and_read = Ocamlbuild_pack.My_unix.run_and_read;;

let ocamlfind_query pkg =
   let cmd = Printf.sprintf "ocamlfind query %s" (Filename.quote pkg) in
   let result = run_and_read cmd in
   try
     let nl = String.rindex result '\n' in
     String.sub result 0 nl
   with Not_found -> result

let (/) = Filename.concat;;
let js_prefix = ocamlfind_query "js_of_ocaml";;
let js_runtime = js_prefix / "runtime";;
let js_runtime f = js_runtime / f;;
let js_tweako = S[Px (js_runtime "runtime.js"); Px (js_runtime "weak.js")];;

dispatch begin function
  | Before_rules ->
    rule "js_of_ocaml: byte -> js"
      ~deps:["lib/dom_react.cmo";"%.byte"]
      ~prod:"%.js"
      begin fun env build ->
        let dst = Filename.dirname (Unix.getcwd () / env "%.js") in
        let src = Filename.dirname (Filename.dirname (Unix.getcwd ()) / env "%.js") / "index.html" in
        Seq [Cmd (S[A"js_of_ocaml"; 
                    A"-pretty"; A"-noruntime"; 
                    js_tweako; Px(env "%.byte")]);
             Cmd (S[A"cp"; Px src; Px dst])]
      end;
    flag ["ocaml"; "byte"; "compile"] (S[A"-I"; P"lib";A"-annot"]);
    flag ["ocaml"; "byte"; "link"; "use_dom_react"] (S[P("lib/dom_react.cmo")]);
  | _ -> ()
end;;
