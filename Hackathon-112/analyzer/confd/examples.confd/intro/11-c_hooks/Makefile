######################################################################
# Example c_hooks
# (C) 2006-2017 Tail-f Systems
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
	@echo "make cli     Start the CONFD Command Line Interface, J-style"
	@echo "make cli-c   Start the CONFD Command Line Interface, C-style"


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

CONFD_FLAGS = --addloadpath $(CONFD_DIR)/etc/confd
START_FLAGS ?=

OSTYPE	= $(shell uname -s)
SRC	= hooks.c
OBJS	= $(SRC:.c=.o)
CFLAGS	+= -D$(OSTYPE)
CFLAGS +=  -I$(CONFD_DIR)/examples.confd/include -DT_LOG_TRACE

all: hooks.fxs hooks.h hooks $(CDB_DIR) ssh-keydir
	@echo "Build complete"

hooks: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LIBS)


######################################################################
clean:	iclean
	-rm -rf hooks.h hooks 2> /dev/null || true
	-rm -rf cli-history 2> /dev/null || true

start:  stop
	$(CONFD) -c confd.conf $(CONFD_FLAGS)
	./hooks $(START_FLAGS) &

######################################################################
stop:
	### Killing any confd daemon and hooks confd agents
	$(CONFD) --stop || true
	$(KILLALL) hooks || true

######################################################################
cli: cli-j

cli-j:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit

cli-c:
	$(CONFD_DIR)/bin/confd_cli -C --user=admin --groups=admin \
		--interactive || echo Exit
