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

ERLDIRS = $(shell ls erlang-lib/*/Makefile | sed 's:/Makefile::')

######################################################################
# Example specific definitions and rules

CONFD_FLAGS = --addloadpath $(CONFD_DIR)/etc/confd
START_FLAGS ?=

FXS_SRC = users.fxs
FXS_HDR_DIR = erlang-lib/ec_transform/include
FXS_HDR = $(FXS_SRC:%.fxs=$(FXS_HDR_DIR)/%.hrl)
AAA_HDR = tailf-aaa.hrl ietf-netconf-acm.hrl

all:	$(FXS_HDR) $(AAA_HDR) ssh-keydir $(CDB_DIR)
	@set -e ; \
	  for d in $(ERLDIRS) ; do \
	    if [ -f $$d/Makefile ]; then \
	      ( cd $$d && $(MAKE) $@ ) || exit 1 ; \
	    fi ; \
	  done
	@echo "Build complete"

$(AAA_HDR): %.hrl: $(CONFD_DIR)/etc/confd/%.fxs
	$(CONFDC) --emit-hrl $@ $<

$(FXS_HDR): $(FXS_SRC)
	$(CONFDC) --emit-hrl $@ $<

######################################################################
clean:	iclean
	@set -e ; \
	  for d in $(ERLDIRS) ; do \
	    if [ -f $$d/Makefile ]; then \
	      ( cd $$d && $(MAKE) $@ ) || exit 1 ; \
	    fi ; \
	  done
	-rm -rf $(FXS_HDR) $(AAA_HDR) 2> /dev/null || true
	-rm -rf *log cli-history 2> /dev/null || true

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
