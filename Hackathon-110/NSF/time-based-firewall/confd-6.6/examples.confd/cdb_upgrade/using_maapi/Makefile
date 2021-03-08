######################################################################
# CDB Upgrade Using MAAPI example
# (C) 2006-2009 Tail-f Systems
#
# See the README file for more information
######################################################################

usage:
	@echo "See README file for more instructions"
	@echo "make all      Build all example files"
	@echo "make clean    Remove all built and intermediary files"
	@echo "make start    Start ConfD with schema version 1"
	@echo "make start-v2 Start ConfD with schema version 2"
	@echo "make stop     Stop any ConfD daemon and example agent"
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

all:	ssh-keydir
	@echo "Build complete"

server_upgrade: server_upgrade.c servers2.h
	$(CC) $(CFLAGS) $< $(LIBS) -o $@

######################################################################
clean: iclean
	-rm -rf confd_candidate.db \
		server_upgrade servers2.h 2> /dev/null || true

######################################################################
start:  stop $(CDB_DIR) servers.fxs
	: ### Start the confd daemon using servers.fxs
	cp servers_init.xml $(CDB_DIR)/servers_init.xml
	$(CONFD) -c confd.conf $(CONFD_FLAGS)
	: ### CDB will be initialized with servers_init.xml
	: ### Inspect using "$(MAKE) query"
	: ### Edit entries using command line interface "$(MAKE) cli"
	: ### Then stop confd and start version 2, using "$(MAKE) start-v2"

start-v2: stop server_upgrade servers2.fxs
	@if [ ! -e $(CDB_DIR)/C.cdb ]; then			\
	    echo "Run \"$(MAKE) start\" before start-v2" ;	\
	    exit 1 ;						\
	 fi
	: ### Remove the fxs file which is no longer valid
	rm servers.fxs
	: ### Remove the init file which is no longer valid
	rm $(CDB_DIR)/servers_init.xml
	: ### Start the confd daemon using servers2.fxs version 2.0
	$(CONFD) -c confd.conf --start-phase0 $(CONFD_FLAGS)
	: ### Now run "server_upgrade" to move servers to new catagories
	./server_upgrade
	: ### Now bring ConfD up to fully operational state
	$(CONFD) --start-phase1
	$(CONFD) --start-phase2
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
	$(CONFD_DIR)/bin/netconf-console -snoaaa --get

######################################################################
