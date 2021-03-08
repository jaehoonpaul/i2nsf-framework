######################################################################
# Interface Status example
# (C) 2006-2009 Tail-f Systems
#
# See the README file for more information
######################################################################

usage:
	@echo "See README file for more instructions"
	@echo "make all     Build all example files"
	@echo "make clean   Remove all built and intermediary files"
	@echo "make start   Start CONFD daemon and example agent"
	@echo "make stop    Stop any CONFD daemon and example agent"
	@echo "make query   Run query against CONFD"
	@echo "make cli     Start the CONFD Command Line Interface"

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

PROG	= ifstatus
SRC	= $(PROG).c
OBJS	= $(SRC:.c=.o)
LIBS	+= -lcrypto

all:	linux if.fxs $(PROG) $(CDB_DIR) $(CDB_DIR)/if_init.xml ssh-keydir
	@echo "Build complete"

$(PROG): $(OBJS)
	$(CC) -o $@ $(OBJS) $(LIBS)

$(PROG).o: if.h

$(CDB_DIR):
	-mkdir -p $(CDB_DIR) 2>/dev/null || true
	cp $(CONFD_DIR)/var/confd/cdb/aaa_init.xml $(CDB_DIR)

######################################################################
clean:	iclean
	-rm -rf if.h $(PROG) *log *trace > /dev/null || true

######################################################################
$(CDB_DIR)/if_init.xml: if_init.xml
	cp if_init.xml $@

start:  stop
	### Start the confd daemon with our example specific confd-config
	$(CONFD) -c confd.conf $(CONFD_FLAGS)
	### In another terminal window, start the CLI (make cli)
	### Starting the ifstatus agent
	./$(PROG) $(START_FLAGS)

######################################################################
stop:
	### Killing any confd daemon and ifstatus confd agents
	$(CONFD) --stop    || true
	$(KILLALL) $(PROG) || true

######################################################################
cli:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit

######################################################################
query:
	$(CONFD_DIR)/bin/netconf-console cmd-get-interfaces.xml

######################################################################
