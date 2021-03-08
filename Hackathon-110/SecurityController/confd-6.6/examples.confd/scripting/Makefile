######################################################################
# (C) 2013 Tail-f Systems
#
# See the README file for more information
######################################################################

usage:
	@echo "See README file for more instructions"
	@echo "make all     Build all example files"
	@echo "make clean   Remove all built and intermediary files"
	@echo "make start   Start CONFD daemon and example agent"
	@echo "make stop    Stop any CONFD daemon and example agent"
	@echo "make cli     Start the CONFD Command Line Interface"
	@echo "make cli-c   Start the CONFD Command Line Interface, C-style"
	@echo "make cli-j   Start the CONFD Command Line Interface, J-style"

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

CONFD_FLAGS = --addloadpath $(CONFD_FXS_DIR) --ignore-initial-validation
START_FLAGS ?=

OSTYPE	= $(shell uname -s)
CFLAGS	+= -D$(OSTYPE)
TARGETS = \
	$(CDB_DIR) \
	$(CDB_DIR)/simple_init.xml \
	ssh-keydir \
	simple.fxs \
	confd.c.ccl \
	confd.j.ccl

all:	$(TARGETS)
	@echo "Build complete"

$(CDB_DIR)/%_init.xml: %_init.xml
	cp $< $@

######################################################################
clean:	iclean
	-rm -rf $(TARGETS) 2> /dev/null || true
	-rm -rf *log *trace cli-history 2> /dev/null || true

start:  stop
	$(CONFD) -c confd.conf $(CONFD_FLAGS)

######################################################################
stop:
	### Killing confd daemon
	$(CONFD) --stop || true

######################################################################
cli:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit

cli-c:
	$(CONFD_DIR)/bin/confd_cli -C --user=admin --groups=admin \
		--interactive || echo Exit

cli-j:
	$(CONFD_DIR)/bin/confd_cli -J --user=admin --groups=admin \
		--interactive || echo Exit

######################################################################
