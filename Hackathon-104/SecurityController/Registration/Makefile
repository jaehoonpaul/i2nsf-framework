######################################################################
# Introduction example 1-2-3-start-query-model
# (C) 2006 Tail-f Systems
#
# See the README files for more information
######################################################################

usage:
	@echo "See README files for more instructions"
	@echo "make all     Build all example files"
	@echo "make py-all     Build all example files for Python"
	@echo "make clean   Remove all built and intermediary files"
	@echo "make start   Start CONFD daemon and example agent"
	@echo "make py-start   Start CONFD daemon and example Python agent"
	@echo "make stop    Stop any CONFD daemon and example agent"
	@echo "make query   Run query against CONFD"
	@echo "make cli     Start the CONFD Command Line Interface, J-style"
	@echo "make cli-c   Start the CONFD Command Line Interface, C-style"

######################################################################
# Where is ConfD installed? Make sure CONFD_DIR points it out
CONFD_DIR ?= ../confd-6.6/

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

SRC	= ietf_i2nsf_reg_interface.c main.c
OBJS	= $(SRC:.c=.o)

all:	ietf-i2nsf-reg-interface.fxs ietf-i2nsf-capability.fxs ietf-i2nsf-capability.h sc-server commands-j.ccl commands-c.ccl \
		$(CDB_DIR) ssh-keydir
	@echo "Build complete"

py-all:	dhcpd.fxs dhcpd_ns.py commands-j.ccl commands-c.ccl \
		$(CDB_DIR) ssh-keydir
	@echo "Build complete"

ietf_i2nsf_reg_interface.o: ietf-i2nsf-reg-interface.h 
#ietf_i2nsf_capability.o: ietf-i2nsf-capability.h
sc-server: $(OBJS)
	$(CC) -o ./sc-server $(OBJS) $(LIBS)


######################################################################
clean:	iclean
	-rm -rf ./sc-server ietf-i2nsf-reg-interface.h ietf-i2nsf-capability.h dhcpd_conf dhcpd.conf *_ns.py *.pyc 2> /dev/null || true

######################################################################
start:  stop start_confd start_subscriber

start_confd:
	$(CONFD) -c confd.conf $(CONFD_FLAGS)

start_subscriber:
	### * In one terminal window, run: tail -f ./confd.log
	### * In another terminal window, run queries
	###   (try 'make query' for an example)
	### * In this window, the DHCP confd daemon now starts:
	###   (hit Enter to exit)
	./sc-server $(START_FLAGS)

######################################################################
py-start:  stop start_confd start_py_subscriber

dhcpd_ns.py: dhcpd.fxs
	$(CONFDC) --emit-python dhcpd_ns.py dhcpd.fxs

start_py_subscriber:
	### * In one terminal window, run: tail -f ./confd.log
	### * In another terminal window, run queries
	###   (try 'make query' for an example)
	### * In this window, the DHCP confd daemon now starts:
	###   (hit Ctrl-c to exit)
	python ./dhcpd_conf.py $(START_FLAGS)

######################################################################
stop:
	### Killing any confd daemon or DHCP confd agents
	$(CONFD) --stop    || true
	$(KILLALL) dhcpd_conf || true

######################################################################
cli:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit

cli-c:
	$(CONFD_DIR)/bin/confd_cli -C --user=admin --groups=admin \
		--interactive  || echo Exit

######################################################################
query:
	$(CONFD_DIR)/bin/netconf-console-tcp cmd-get-dhcpd.xml

######################################################################
