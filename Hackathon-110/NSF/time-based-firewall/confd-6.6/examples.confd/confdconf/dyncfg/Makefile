######################################################################
# Interface Status example
# (C) 2008 Tail-f Systems
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

CONFD_FLAGS = --addloadpath $(CONFD_FXS_DIR)
START_FLAGS ?=


all:	confd_dyncfg.fxs $(CDB_DIR) ssh-keydir $(CDB_DIR)/confd_dyncfg_init.xml
	@echo "Build complete"

$(CDB_DIR)/confd_dyncfg_init.xml: confd_dyncfg_init.xml
	rm -f $@ ; cp $< $@


confd_dyncfg.fxs: $(CONFD_DIR)/src/confd/dyncfg/confd_dyncfg.fxs
	cp $< $@

######################################################################
clean:	iclean
	-rm -rf $(CS).cs
	--rm -rf *log *trace cli-history 2> /dev/null || true

######################################################################

start:  stop
	: ### Start the confd daemon with our example specific confd-config
	$(CONFD) -c confd.conf $(CONFD_FLAGS)

######################################################################
stop:
	: ### Stopping any currently running confd daemon
	$(CONFD) --stop    || true

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
