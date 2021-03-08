######################################################################
# SNMP Agent example "simple" Makefile
# (C) 2006 Tail-f Systems
#
# See the README files for more information
######################################################################

usage:
	@echo "See README file for more instructions"
	@echo "make all      Build all example files"
	@echo "make clean    Remove all built and intermediary files"
	@echo "make start    Start ConfD daemon and example agent"
	@echo "make stop     Stop any ConfD daemon and example agent"
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

all:	system.fxs $(CDB_DIR)
	@echo "Build complete"


$(CDB_DIR):
	-mkdir -p $(CDB_DIR) 2>/dev/null || true
	cp ./aaa_init.xml system.xml $(CDB_DIR)


######################################################################
clean:	iclean
	-rm -rf *log *trace cli-history 2> /dev/null || true

start:  stop
	$(CONFD) -c ./confd.conf  $(CONFD_FLAGS)


######################################################################
stop:
	### Killing any confd daemon
	$(CONFD) --stop    || true

######################################################################
cli:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit

ccli:
	$(CONFD_DIR)/bin/confd_cli -C --user=admin --groups=admin \
		--interactive || echo Exit


######################################################################
cli-c:
	$(CONFD_DIR)/bin/confd_cli -C --user=admin --groups=admin \
		--interactive || echo Exit

######################################################################
