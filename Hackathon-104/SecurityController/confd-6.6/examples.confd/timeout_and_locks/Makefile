######################################################################
# Introduction example 1-2-3-start-query-model
# (C) 2006 Tail-f Systems
#
# See the README files for more information
######################################################################

usage:
	@echo "See README files for more instructions"
	@echo "make all     Build all example files"
	@echo "make clean   Remove all built and intermediary files"
	@echo "make start   Start CONFD daemon and example agent"
	@echo "make stop    Stop any CONFD daemon and example agent"
	@echo "make query   Run query against CONFD"
	@echo "make cli     Start the CONFD Command Line Interface, J-style"
	@echo "make cli-c   Start the CONFD Command Line Interface, C-style"

######################################################################
# Where is ConfD installed? Make sure CONFD_DIR points it out
CONFD_DIR ?= ../..

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

DSRC	= dhcpd_conf.c
DOBJS	= $(DSRC:.c=.o)

ASRC	= arpstat.c
AOBJS	= $(ASRC:.c=.o)

JAVAC = javac
JAVA  = java

all: c-all java-all
	@echo "Build complete"

common-all: dhcpd.fxs commands-j.ccl commands-c.ccl $(CDB_DIR) ssh-keydir \
	arpe.fxs arpe.h

c-all: common-all dhcpd_conf arpstat
	@echo "C build complete"

java-all: common-all Actions.class slowTrans.class
	@echo "Java build complete"

%.class:	%.java
	$(JAVAC) -classpath $(CLASSPATH):$(JLIB):. $*.java

%.java: %.fxs
	$(CONFDC) --emit-java $*.java $<

dhcpd_conf.o: dhcpd.h
dhcpd_conf: $(DOBJS)
	$(CC) -o $@ $(DOBJS) $(LIBS)

arpstat.o: arpe.h
arpstat: $(AOBJS)
	$(CC) -o $@ $(AOBJS) $(LIBS)

######################################################################
clean:	iclean
	-rm -rf arpstat arpe.h dhcpd.h dhcpd_conf *.class dhcpd.conf 2> /dev/null || true
	-rm -rf *log *trace cli-history 2> /dev/null || true

######################################################################
start:  stop start_confd start_subscriber start_java_action start_arpe

start_confd:
	$(CONFD) -c confd.conf $(CONFD_FLAGS)

start_subscriber:
	### * In one terminal window, run: tail -f ./confd.log
	### * In another terminal window, run queries
	###   (try 'make query' for an example)
	### * In this window, the DHCP confd daemon now starts:
	###   (hit Enter to exit)
	./dhcpd_conf $(START_FLAGS) &

start_arpe:
	PATH=/sbin:/usr/sbin:$$PATH ./arpstat $(START_FLAGS) &

start_java_action:
	$(JAVA) -classpath  $(CLASSPATH):$(JLIB):. Actions $(START_FLAGS) &

######################################################################
stop:
	### Killing any confd daemon or DHCP confd agents
	$(CONFD) --stop    || true
	ps ux | grep 'java.*Actions' | grep -v grep | cut -f 2 -d ' ' | \
		xargs kill -9 || true
	ps ux | grep 'java.*slowTrans' | grep -v grep | cut -f 2 -d ' ' | \
		xargs kill -9 || true
	$(KILLALL) dhcpd_conf || true
	$(KILLALL) arpstat || true

######################################################################
cli:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit

cli-c:
	$(CONFD_DIR)/bin/confd_cli -C --user=admin --groups=admin \
		--interactive  || echo Exit

######################################################################
query:
	$(CONFD_DIR)/bin/netconf-console cmd-get-dhcpd.xml

######################################################################
