######################################################################
# NETCONF Extension example
# (C) 2006 Tail-f Systems
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
CONFD_DIR ?= ../../..

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

SRC       = event.c
OBJS      = $(SRC:.c=.o)

LIBS	+= -lexpat

all:	event event-rpc.fxs $(CDB_DIR) ssh-keydir
	@echo "Build complete"

$(OBJS):	$(SRC)

event:	$(OBJS)
	 $(CC) $(OBJS) $(LIBS) $(CFLAGS) -o $@

######################################################################
clean:	iclean
	-rm -rf event event-rpc.fxs 2> /dev/null || true
	-rm -rf *log *trace cli-history 2> /dev/null || true

start:  stop
	$(CONFD) -c ./confd.conf $(CONFD_FLAGS)

######################################################################
stop:
	### Stopping any confd daemon
	$(CONFD) --stop || true

######################################################################
cli:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit

######################################################################
query:
	$(CONFD_DIR)/bin/netconf-console -s raw cmd-event.xml

######################################################################
