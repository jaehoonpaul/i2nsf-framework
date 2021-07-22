###-*-makefile-*-   ; force emacs to enter makefile-mode

ERL       =  erl
ERLC      =  erlc
CONFD     =  confd
CONFDC     = confdc


ERLC_FLAGS=-W 

ifeq ($(TYPE), debug)
  ERLC_FLAGS+=+debug_info -Ddebug
endif

ifeq ($(TYPE), dialyzer)
  ERLC_FLAGS+=+debug_info
endif

ERL_SOURCES  := $(wildcard *.erl)
ERL_HEADERS += $(wildcard *.hrl) $(wildcard ../include/*.hrl) 
ERL_OBJECTS := $(ERL_SOURCES:%.erl=../ebin/%.beam) 
MODULES     := $(ERL_SOURCES:%.erl=%)


APP_SOURCES := $(wildcard *.app.src)
APP_OBJECTS := $(APP_SOURCES:%.app.src=../ebin/%.app)

APPNAME := $(APP_SOURCES:%.app.src=%)


APPSCRIPT = '$$vsn=shift; $$mods=""; while(@ARGV){ $$_=shift; s/^([A-Z].*)$$/\'\''$$1\'\''/; $$mods.=", " if $$mods; $$mods .= $$_; } while(<>) { s/%VSN%/$$vsn/; s/%MODULES%/$$mods/; print; }'

EDOC_OPTS=[{dir,"../doc/"},{todo,true}]

# Erlang Targets

../ebin/%.app: %.app.src ../confdvsn.mk Makefile
	perl -e $(APPSCRIPT) "$(CONFDVSN)" $(MODULES) < $< > $@

../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $<

../ebin/%.beam: %.yrl
	$(ERLC) $<
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $*.erl
	rm -f $*.erl

