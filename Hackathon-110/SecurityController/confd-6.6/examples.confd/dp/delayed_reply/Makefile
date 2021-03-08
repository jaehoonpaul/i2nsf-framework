######################################################################
# ConfD delayed data provider reply example
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

SHORT_TIMEOUT = PT1S
LONG_TIMEOUT = PT4S

OSTYPE	= $(shell uname -s)
SRC	= delayed_dp.c
OBJS	= $(SRC:.c=.o)
CFLAGS	+= -D$(OSTYPE)

all:	routes.fxs routes.h delayed_dp $(CDB_DIR) ssh-keydir
	@echo "Build complete"

delayed_dp: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LIBS)


######################################################################
clean:	iclean
	-rm -rf routes.h delayed_dp confd.conf 2> /dev/null || true

start:  stop
	sed -e 's|<!-- %QUERY-TIMEOUT% -->|$(LONG_TIMEOUT)|' < confd.conf.in > confd.conf
	$(CONFD) -c confd.conf $(CONFD_FLAGS)
	### * In one terminal window, run: tail -f ./confd.log
	### * In another terminal window, run queries
	###   (try 'make query' for an example)
	### * In this window, the delayed_dp application now starts:
	PATH=/sbin:/usr/sbin:$$PATH ./delayed_dp $(START_FLAGS)

reload-fail:
	sed -e 's|<!-- %QUERY-TIMEOUT% -->|$(SHORT_TIMEOUT)|' < confd.conf.in > confd.conf
	$(CONFD) --reload

######################################################################
stop:
	### Killing any confd daemon and delayed_dp confd agents
	$(CONFD) --stop || true
	$(KILLALL) delayed_dp || true

######################################################################
cli:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit

cli-c:
	$(CONFD_DIR)/bin/confd_cli -C --user=admin --groups=admin \
		--interactive || echo Exit

######################################################################
query:
	$(CONFD_DIR)/bin/netconf-console-tcp --get -x /routes/route[id='17']/value

######################################################################
