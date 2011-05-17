
## Sample Makefile for eliom application.

APP_NAME := gallery

## Package required to build the server part of the application

SERVER_PACKAGE := cairo

## Package to be linked in the client part

CLIENT_PACKAGE :=

## Source files for the server part

SERVER_FILES := ${wildcard *.eliom}

## Source files for the client part

CLIENT_FILES := react_dom.ml ${wildcard *.eliom}

## Needed binaries

ELIOMC      := eliomc -annot
ELIOMOPT    := eliomopt -annot
ELIOMDEP    := eliomdep -annot
JS_OF_ELIOM := js_of_eliom

## Where to put intermediate object files.
## - ELIOM_{SERVER,CLIENT}_DIR must be distinct
## - ELIOM_CLIENT_DIR mustn't be the local dir.
## - ELIOM_SERVER_DIR could be ".", but you need to
##   remove it from the "clean" rules...

export ELIOM_SERVER_DIR := _server
export ELIOM_CLIENT_DIR := _client
export ELIOM_TYPE_DIR   := .

#####################################

all: byte opt
	echo "reload" > /home/spec/prog/ocsigenserver-1.91/local/var/run/ocsigenserver_command
byte:: ${APP_NAME}.cma ${APP_NAME}.js
opt:: ${APP_NAME}.cmxs ${APP_NAME}.js

#### Server side compilation #######

SERVER_INC  := ${addprefix -package ,${SERVER_PACKAGE}}

SERVER_OBJS := $(patsubst %.eliom,${ELIOM_SERVER_DIR}/%.cmo, ${SERVER_FILES})
SERVER_OBJS := $(patsubst %.ml,${ELIOM_SERVER_DIR}/%.cmo, ${SERVER_OBJS})

${APP_NAME}.cma: ${SERVER_OBJS}
	${ELIOMC} -a -o $@ $^
${APP_NAME}.cmxa: ${SERVER_OBJS:.cmo=.cmx}
	${ELIOMOPT} -a -o $@ $^

${ELIOM_TYPE_DIR}/%.type_mli: %.eliom
	${ELIOMC} -infer ${SERVER_INC} $<

${ELIOM_SERVER_DIR}/%.cmi: %.mli
	${ELIOMC} -c ${SERVER_INC} $<

${ELIOM_SERVER_DIR}/%.cmo: %.ml
	${ELIOMC} -c ${SERVER_INC} $<
${ELIOM_SERVER_DIR}/%.cmo: %.eliom
	${ELIOMC} -c -noinfer ${SERVER_INC} $<

${ELIOM_SERVER_DIR}/%.cmx: %.ml
	${ELIOMOPT} -c ${SERVER_INC} $<
${ELIOM_SERVER_DIR}/%.cmx: %.eliom
	${ELIOMOPT} -c -noinfer ${SERVER_INC} $<

%.cmxs: %.cmxa
	$(ELIOMOPT) -shared -linkall -o $@ $<

##### Client side compilation ####

CLIENT_LIBS := ${addprefix -package ,${CLIENT_PACKAGE}}
CLIENT_INC  := ${addprefix -package ,${CLIENT_PACKAGE}}

CLIENT_OBJS := $(patsubst %.eliom,${ELIOM_CLIENT_DIR}/%.cmo, ${CLIENT_FILES})
CLIENT_OBJS := $(patsubst %.ml,${ELIOM_CLIENT_DIR}/%.cmo, ${CLIENT_OBJS})

${APP_NAME}.js: ${CLIENT_OBJS}
	${JS_OF_ELIOM} -o $@ ${CLIENT_LIBS} $^

${ELIOM_CLIENT_DIR}/%.cmi: %.mli
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<

${ELIOM_CLIENT_DIR}/%.cmo: %.eliom
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<
${ELIOM_CLIENT_DIR}/%.cmo: %.ml
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<

############

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f *.type_mli
	-rm -f ${APP_NAME}.js
	-rm -rf ${ELIOM_CLIENT_DIR} ${ELIOM_SERVER_DIR}

distclean: clean.local
	-rm -f *~ \#* .\#*

## Dependencies

depend:
	$(ELIOMDEP) -server ${SERVER_INC} ${SERVER_FILES} > .depend
	$(ELIOMDEP) -client ${CLIENT_INC} ${CLIENT_FILES} >> .depend

## Warning: Dependencies towards *.eliom are not handled by eliomdep yet.

include .depend

## installation #########

STATICDIR      := /tmp/static

$(STATICDIR):
	mkdir -p $@

install: all $(STATICDIR)
	cp $(APP_NAME).js $(STATICDIR)/$(APP_NAME).js
