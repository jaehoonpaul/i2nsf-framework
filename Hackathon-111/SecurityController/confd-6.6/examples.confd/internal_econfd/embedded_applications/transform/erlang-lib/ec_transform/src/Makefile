include ../vsn.mk

CONFD_DIR ?= ../../../../../../..

APP_DIR    = $(shell dirname $(PWD))
APP_NAME   = $(shell basename $(APP_DIR))
ERL_FILES  = $(wildcard *.erl)
HRL_FILES  = $(wildcard *.hrl) $(wildcard ../*.hrl)
MODULES    = $(ERL_FILES:%.erl=%)
EBIN_FILES = $(MODULES:%=../ebin/%.beam) ../ebin/$(APP_NAME).app
AAA_DIR    = ../../..
AAA_HRL    = $(AAA_DIR)/tailf-aaa.hrl $(AAA_DIR)/ietf-netconf-acm.hrl

ERLC       = erlc
ERLC_FLAGS := \
	+warnings_as_errors \
	+debug_info \
	-pa $(CONFD_DIR)/erlang/econfd/ebin \
	-pa ../../ec_transform/ebin \
	-I $(AAA_DIR)

APPSCRIPT = '$$vsn=shift; $$mods=""; while(@ARGV){ $$_=shift; s/^([A-Z].*)$$/\'\''$$1\'\''/; $$mods.=", " if $$mods; $$mods .= $$_; } while(<>) { s/%VSN%/$$vsn/; s/%MODULES%/$$mods/; print; }'

all: $(EBIN_FILES)

clean:
	rm -f $(EBIN_FILES) erl_crash.dump core* *~

../ebin/%.app: %.app.src ../vsn.mk Makefile
	perl -e $(APPSCRIPT) "$(VSN)" $(MODULES) < $< > $@

../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $<

$(EBIN_FILES): $(ERL_FILES) $(HRL_FILES) ../ebin $(AAA_HRL)
