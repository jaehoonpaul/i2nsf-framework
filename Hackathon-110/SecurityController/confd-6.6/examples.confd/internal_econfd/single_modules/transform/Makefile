######################################################################
# AAA-Transform example
# (C) 2012 Tail-f Systems
#
# See the README file for more information
######################################################################

usage:
	@echo "See README file for more instructions"
	@echo "make all      Build all example files"
	@echo "make clean    Remove all built and intermediary files"
	@echo "make start    Start ConfD daemon and example agent"
	@echo "make stop     Stop any ConfD daemon and example agent"
	@echo "make query    Run NETCONF query against ConfD"
	@echo "make cli      Start the ConfD Command Line Interface"

######################################################################
# Where is ConfD installed? Make sure CONFD_DIR points it out
CONFD_DIR ?= ../../../..

# Include standard ConfD build definitions and rules
include $(CONFD_DIR)/src/confd/build/include.mk

# In case CONFD_DIR is not set (correctly), this rule will trigger
$(CONFD_DIR)/src/confd/build/include.mk:
	@echo 'Where is ConfD installed? Set $$CONFD_DIR to point it out!'
	@echo ''
	@exit 1

######################################################################
# Example specific definitions and rules

CONFD_FLAGS = --addloadpath $(CONFD_DIR)/etc/confd
START_FLAGS ?=

FXSS	= users.fxs
SRC	= ec_users_aaa.erl
OBJ     = $(SRC:%.erl=%.beam)
HDR     = $(FXSS:%.fxs=%.hrl)
AAA_HDR = tailf-aaa.hrl ietf-netconf-acm.hrl

ERLC       = erlc
ERLC_FLAGS = -I $(CONFD_DIR)/erlang/econfd/include

all:	$(FXSS) $(HDR) $(AAA_HDR) $(OBJ) ssh-keydir $(CDB_DIR)
	@echo "Build complete"

%.beam: %.erl $(HDR) $(AAA_HDR)
	$(ERLC) $(ERLC_FLAGS) $<

$(AAA_HDR): %.hrl: $(CONFD_DIR)/etc/confd/%.fxs
	$(CONFDC) --emit-hrl $@ $<

%.hrl: %.fxs
	$(CONFDC) --emit-hrl $*.hrl $*.fxs

######################################################################
clean:	iclean
	-rm -rf ec_users_aaa.beam *.hrl 2> /dev/null || true

start:  stop
	$(CONFD) -c confd.conf $(CONFD_FLAGS)

######################################################################
stop:
	$(CONFD) --stop    || true

######################################################################
cli:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit

######################################################################
cli-c:
	$(CONFD_DIR)/bin/confd_cli -C --user=admin --groups=admin \
		--interactive || echo Exit

######################################################################
query:
	$(CONFD_DIR)/bin/netconf-console -u admin --get-config

######################################################################
