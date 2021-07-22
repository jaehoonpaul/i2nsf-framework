######################################################################
# CDB Upgrade Simple example
# (C) 2006 Tail-f Systems
#
# See the README file for more information
######################################################################

usage:
	@echo "See README file for more instructions"
	@echo "make all      Build all example files"
	@echo "make clean    Remove all built and intermediary files"
	@echo "make start    Start ConfD with schema version 1"
	@echo "make start-v2 Start ConfD with schema version 2"
	@echo "make stop     Stop the ConfD daemon"
	@echo "make query    Run query against ConfD"
	@echo "make cli      Start the ConfD Command Line Interface"

######################################################################
# Where is ConfD installed? Make sure CONFD_DIR points it out
CONFD_DIR ?= ../../..

# Include standard ConfD build definitions and rules
include $(CONFD_DIR)/src/confd/build/include.mk

# In case CONFD_DIR is not set (correctly), this rule will trigger
$(CONFD_DIR)/src/confd/build/include.mk:
	@echo 'Where is ConfD installed? Set $$CONFD_DIR to point it out!'
	@echo ''

######################################################################
# Example specific definitions and rules

CONFD_FXS_DIR = $(CONFD_DIR)/etc/confd

CONFD_FLAGS = --addloadpath $(CONFD_FXS_DIR)
START_FLAGS ?=

all:	$(CDB_DIR) ssh-keydir
	@echo "Build complete"

######################################################################
clean: iclean
	-rm -rf confd_candidate.db 2> /dev/null || true

######################################################################
start:  stop $(CDB_DIR) simple.fxs
	: ### Start the confd daemon using simple.fxs
	cp simple_init.xml $(CDB_DIR)/simple_init.xml
	$(CONFD) -c confd.conf $(CONFD_FLAGS)
	: ### CDB will be initialized with simple_init.xml
	: ### Inspect using "$(MAKE) query"
	: ### Edit entries using command line interface "$(MAKE) cli"
	: ### Then stop confd and start version 2, using "$(MAKE) start-v2"

start-v2:  stop simple2.fxs
	: ### Remove the fxs file which is no longer valid
	rm simple.fxs
	: ### Remove the init file which is no longer valid
	rm $(CDB_DIR)/simple_init.xml
	: ### Start the confd daemon using simple.fxs version 2
	$(CONFD) -c confd.conf $(CONFD_FLAGS)
	: ### To see the result query the daemon using "$(MAKE) query"

######################################################################
stop:
	: ### Stopping any confd daemon running
	$(CONFD) --stop    || true

######################################################################
cli:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit

######################################################################
query:
	$(CONFD_DIR)/bin/netconf-console -snoaaa --get-config --with-defaults=report-all-tagged

######################################################################
