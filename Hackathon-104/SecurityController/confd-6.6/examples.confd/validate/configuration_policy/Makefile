######################################################################
# Configuration Policy example
# (C) 2010 Tail-f Systems
#
# See the README file for more information
######################################################################

usage:
	@echo "See README file for more instructions"
	@echo "make all      Build all example files"
	@echo "make clean    Remove all built and intermediary files"
	@echo "make start    Start ConfD daemon and example agent"
	@echo "make stop     Stop any ConfD daemon and example agent"
	@echo "make cli      Start the ConfD Command Line Interface"
	@echo "make c_cli    Start the ConfD Command Line Interface"

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


all:	config.fxs tailf-configuration-policy.fxs \
		$(CDB_DIR) $(CDB_DIR)/init.xml ssh-keydir
	@echo "Build complete"

tailf-configuration-policy.fxs: $(CONFD_DIR)/src/confd/configuration_policy/tailf-configuration-policy.fxs
	cp $< $@

clean:	iclean
	rm -f xpath.trace

# Make sure ConfD can find the 'maapi' program
start:  stop
	env PATH=$(CONFD_DIR)/bin:$$PATH \
	    $(CONFD) -c ./confd.conf $(CONFD_FLAGS); 


######################################################################

$(CDB_DIR)/%.xml: %.xml
	rm -f $@ ; cp $< $@

######################################################################
stop:
	$(CONFD) --stop || true

cli:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit

c_cli:
	$(CONFD_DIR)/bin/confd_cli -C --user=admin --groups=admin \
		--interactive || echo Exit


######################################################################
cli-c:
	$(CONFD_DIR)/bin/confd_cli -C --user=admin --groups=admin \
		--interactive || echo Exit
