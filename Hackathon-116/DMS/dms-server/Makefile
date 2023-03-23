######################################################################
# Introduction example 1-2-3-start-query-model
# (C) 2006 Tail-f Systems
#
# See the README files for more information
######################################################################

usage:
	@echo "See README files for more instructions"
	@echo "make all     Build all example files for Python"
	@echo "make clean   Remove all built and intermediary files"
	@echo "make start   Start CONFD daemon and example Python agent"
	@echo "make stop    Stop any CONFD daemon and example agent"
	@echo "make query   Run query against CONFD"
	@echo "make cli     Start the CONFD Command Line Interface, J-style"
	@echo "make cli-c   Start the CONFD Command Line Interface, C-style"

######################################################################
# Where is ConfD installed? Make sure CONFD_DIR points it out
CONFD_DIR ?= ../../..

# Include standard ConfD build definitions and rules
include $(CONFD_DIR)/src/confd/build/include.mk
NETCONF_CONSOLE ?= $(CONFD_DIR)/bin/netconf-console

# In case CONFD_DIR is not set (correctly), this rule will trigger
$(CONFD_DIR)/src/confd/build/include.mk:
	@echo 'Where is ConfD installed? Set $$CONFD_DIR to point it out!'
	@echo ''

######################################################################
# Example specific definitions and rules

CONFD_FLAGS = --addloadpath $(CONFD_DIR)/etc/confd
START_FLAGS ?=

common-all: ietf-i2nsf-capability.fxs ietf-i2nsf-registration-interface.fxs dhcpd.fxs commands-j.ccl commands-c.ccl $(CDB_DIR) ssh-keydir

all: common-all dhcpd_ns.py ietf_i2nsf_registration_interface_ns.py ietf_i2nsf_capability_ns.py
	@echo "Python build complete"


######################################################################
clean:	iclean
	-rm -rf dhcpd.conf *_ns.py __init__.py *.pyc 2> /dev/null || true

######################################################################
start_confd:
	$(CONFD) -c confd.conf $(CONFD_FLAGS)

######################################################################
start:  stop start_confd start_py_subscriber

dhcpd_ns.py: dhcpd.fxs
	$(CONFDC) --emit-python dhcpd_ns.py dhcpd.fxs

ietf_i2nsf_registration_interface_ns.py: ietf-i2nsf-registration-interface.fxs
	$(CONFDC) --emit-python ietf_i2nsf_registration_interface_ns.py ietf-i2nsf-registration-interface.fxs

ietf_i2nsf_capability_ns.py: ietf-i2nsf-capability.fxs
	$(CONFDC) --emit-python ietf_i2nsf_capability_ns.py ietf-i2nsf-capability.fxs

start_py_subscriber:
	### * In one terminal window, run: tail -f ./confd.log
	### * In another terminal window, run queries
	###   (try 'make query' for an example)
	### * In this window, the DHCP confd daemon now starts:
	###   (hit Ctrl-c to exit)
	$(PYTHON) ./ietf_i2nsf_registration_interface_conf.py $(START_FLAGS)

######################################################################
stop:
	### Killing any confd daemon or DHCP confd agents
	$(CONFD) --stop    || true
	$(KILLALL) `pgrep -f "$(PYTHON) ./dhcpd_conf.py"` || true

######################################################################
cli:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit

cli-c:
	$(CONFD_DIR)/bin/confd_cli -C --user=admin --groups=admin \
		--interactive  || echo Exit

######################################################################
query:
	$(NETCONF_CONSOLE) cmd-get-dhcpd.xml

######################################################################
