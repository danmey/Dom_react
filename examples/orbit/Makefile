## Sample Makefile for eliom application.

APP_NAME := orbit

CLIENT_FILES :=  orbit.ml
## Needed binaries
BUILD_DIR := _build
JS_OF_OCAML := js_of_ocaml
JS_OF_ELIOM := js_of_eliom
OCAMLFIND := ocamlfind
LIB_FILES := react_dom.ml

#####################################

all: byte
	echo "reload" > /home/spec/prog/ocsigenserver-1.91/local/var/run/ocsigenserver_command
byte:: ${APP_NAME}.js

##### Client side compilation ####

CLIENT_OBJS := $(patsubst %.ml,_client/%.cmo, ${CLIENT_FILES})

${APP_NAME}.js: ${CLIENT_OBJS}

_client/%.cmo: %.ml
	cp ../../${LIB_FILES} .
	${JS_OF_ELIOM} ${LIB_FILES} $< -o $@
	-rm -f ${LIB_FILES}


############

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot $(CLIENT_OBJS)
	-rm -f ${APP_NAME}.js
	-rm -rf _client

distclean: clean.local
	-rm -f *~ \#* .\#*


## Warning: Dependencies towards *.eliom are not handled by eliomdep yet.
.depend:
	include .depend

## installation #########

STATICDIR      := /tmp/static

$(STATICDIR):
	mkdir -p $@

install: all $(STATICDIR)
	cp $(APP_NAME).js $(STATICDIR)/$(APP_NAME).js
