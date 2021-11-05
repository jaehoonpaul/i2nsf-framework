######################################################################
# Crypto1 example
# (C) 2006 Tail-f Systems
#
# See the README file for more information
######################################################################

usage:
	@echo "See README file for more instructions"
	@echo "make all     Build all example files"
	@echo "make clean   Remove all built and intermediary files"
	@echo "make start   Start ConfD daemon and example agent"
	@echo "make stop    Stop any ConfD daemon and example agent"
	@echo "make query   Run query against ConfD"
	@echo "make cli     Start the ConfD Command Line Interface"

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

SRC	= crypto_cdb.c
OBJS	= $(SRC:.c=.o)
LIBS	+= -lcrypto

all:	smp.fxs smp.h crypto_cdb $(CDB_DIR) ssh-keydir
	@echo "Build complete"

crypto_cdb: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LIBS)

crypto_cdb.o: smp.h

######################################################################
clean:	iclean
	-rm -rf smp.h crypto_cdb 2> /dev/null || true

######################################################################
start:  stop
	### Start the confd daemon with our example specific confd-config
	$(CONFD) -c confd.conf $(CONFD_FLAGS)
	### In another terminal window, start the CLI (make cli)
	### Starting the decryption client
	./crypto_cdb $(START_FLAGS)

######################################################################
stop:
	### Killing any confd daemon and crypto_cdb confd agents
	$(CONFD) --stop    || true
	$(KILLALL) crypto_cdb || true

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
	$(CONFD_DIR)/bin/netconf-console --get-config

######################################################################
