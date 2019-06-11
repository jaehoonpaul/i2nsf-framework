######################################################################
# (C) 2009 Tail-f Systems
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
INITS         := $(wildcard *_init.xml)
CDB_INITS     := $(addprefix $(CDB_DIR)/,$(INITS))

START_FLAGS ?=

all: build

build: example.fxs example_ns.py $(PROGS) $(CDB_DIR) ssh-keydir $(CDB_INITS)
	rm -f action.log
	@echo "Build complete"

$(CDB_DIR)/%.xml: %.xml
	rm -f $@ ; cp $< $@

#
# Cleanup
#
clean: iclean
	rm -rf example_ns.py* __init__.py 2> /dev/null || true
	rm -rf cli-history
	rm -rf log/*
	rm -f action.log
#
# Dependencies

#
# Convenience targets

start: build
	$(CONFD) -c confd.conf --addloadpath $(CONFD_DIR)/etc/confd
	python ./action.py $(START_FLAGS) 1>action.log


stop:
	$(CONFD_DIR)/bin/confd --stop || true
	$(KILLALL) `pgrep -f "python ./action.py"` || true


status:
	$(CONFD_DIR)/bin/confd --status

cli:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit

cli-c:
	$(CONFD_DIR)/bin/confd_cli -C --user=admin --groups=admin \
		--interactive || echo Exit
