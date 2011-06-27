#!/bin/sh
ocamlfind ocamlc -pp "camlp4o /home/spec/prog/godi-3.12/lib/ocaml/site-lib/js_of_ocaml/pa_js.cmo" -package js_of_ocaml -linkpkg -o gallery.byte gallery.ml &&\
js_of_ocaml gallery.byte

ocamlfind ocamlc -thread -package ocsigenserver -package eliom -package eliom.server ocsigen_auth.ml -a -o ocsigen_auth.cma
#echo reload > /var/run/ocsigen_command
