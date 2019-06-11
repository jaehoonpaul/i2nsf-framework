######################################################################
# CPU Load example
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

PROG	= get_load
SRC	= $(PROG).c
OBJS	= $(SRC:.c=.o)
LIBS	+= -lcrypto

all:	load.fxs $(PROG) load.h $(CDB_DIR) ssh-keydir
	@echo "Build complete"

$(PROG): $(OBJS)
	$(CC) -o $@ $(OBJS) $(LIBS)

$(PROG).o: load.h


######################################################################
clean:	iclean
	-rm -rf load.h $(PROG) load 2>/dev/null || true

######################################################################
start:  stop $(CDB_DIR)
	$(CONFD) -c confd.conf $(CONFD_FLAGS)
	### In another terminal window, start the CLI (make cli)
	### Starting the get_load agent - sampling starts at next even minute
	./$(PROG) $(START_FLAGS)

######################################################################
stop:
	### Killing any confd daemon and get_load confd agent
	$(CONFD) --stop    || true
	$(KILLALL) $(PROG) || true

######################################################################
cli:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit

######################################################################
query:
	$(CONFD_DIR)/bin/netconf-console cmd-get-load.xml

######################################################################
