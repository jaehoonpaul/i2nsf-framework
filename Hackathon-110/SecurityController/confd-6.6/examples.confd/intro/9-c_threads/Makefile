######################################################################
# Introduction example 9-c_threads
# (C) 2006 Tail-f Systems
#
# See the README file for more information
######################################################################

usage:
	@echo "See README file for more instructions"
	@echo "make all     Build all example files"
	@echo "make clean   Remove all built and intermediary files"
	@echo "make start   Start CONFD daemon and example agent"
	@echo "make stop    Stop any CONFD daemon and example agent"
	@echo "make restart Stop and restart the CONFD daemon only"
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
SRC	= thr_daemon.c
OBJS	= $(SRC:.c=.o)

all:	model.fxs thr_daemon $(CDB_DIR) ssh-keydir
	@echo "Build complete"

thr_daemon: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LIBS)

$(OBJS): model.h

######################################################################
clean:	iclean
	-rm -rf model.h thr_daemon 2> /dev/null || true
	-rm -rf *log *trace cli-history 2> /dev/null || true

start:  stop
	$(CONFD) -c confd.conf $(CONFD_FLAGS)
	### * In another terminal window, run the CLI (e.g. 'make cli')
	### * In this window, the data provider daemon now starts:
	./thr_daemon $(START_FLAGS)

######################################################################
stop:
	### Killing any confd daemon and data provider daemons
	$(CONFD) --stop || true
	$(KILLALL) thr_daemon || true

######################################################################
restart:
	### Restarting the confd daemon
	$(CONFD) --stop || true
	sleep 3
	$(CONFD) -c confd.conf $(CONFD_FLAGS)

######################################################################
cli:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit

cli-c:
	$(CONFD_DIR)/bin/confd_cli -C --user=admin --groups=admin \
		--interactive || echo Exit

######################################################################
