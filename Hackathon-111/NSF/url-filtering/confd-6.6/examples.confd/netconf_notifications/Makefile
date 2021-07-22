######################################################################
# NETCONF Notification example
# (C) 2006-2009 Tail-f Systems
#
# See the README file for more information
######################################################################

usage:
	@echo "See README file for more instructions"
	@echo "make all      Build all example files"
	@echo "make clean    Remove all built and intermediary files"
	@echo "make start    Start ConfD daemon and example notifier app using the builtin"
	@echo "              replay store"
	@echo "make start2   Start ConfD daemon and example notifier app using an external"
	@echo "              replay store"
	@echo "make stop     Stop any ConfD daemon and example notifier app"
	@echo "make query    Run NETCONF query against ConfD"
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
CONFD_FLAGS = --addloadpath $(CONFD_DIR)/etc/confd
START_FLAGS ?=


CFLAGS	+= $(EXPAT_INC)
LIBS	+= $(EXPAT_LIB)

all: notifier notifier_builtin_replay_store notif.fxs $(CDB_DIR) ssh-keydir
	@echo "Build complete"

notifier: notifier.o
	 $(CC) notifier.o $(LIBS) $(CFLAGS) -ansi -pedantic -o $@

notifier.o: notifier.c notif.h

notifier_builtin_replay_store: notifier_builtin_replay_store.o
	 $(CC) notifier_builtin_replay_store.o $(LIBS) $(CFLAGS) -o $@

notifier_builtin_replay_store.o: notifier_builtin_replay_store.c notif.h

%.h: %.fxs
	$(CONFDC) --emit-h $*.h $<

######################################################################
clean:	iclean
	-rm -rf notifier notifier_builtin_replay_store *.h interface.*\
		confd_prim.conf 2> /dev/null || true

start:  stop
	sed 's/[ \t]*<!-- enableBuiltinReplayStore -->/          <enabled>true<\/enabled>/' confd.conf > confd_prim.conf
	$(CONFD) -c ./confd_prim.conf $(CONFD_FLAGS)
	LD_LIBRARY_PATH=$(CONFD_SO) ./notifier_builtin_replay_store -t

startw:  stop
	sed 's/[ \t]*<!-- enableBuiltinReplayStore -->/          <enabled>true<\/enabled>/' confd.conf > confd_prim.conf
	$(CONFD) -c ./confd_prim.conf $(CONFD_FLAGS) -w
	LD_LIBRARY_PATH=$(CONFD_SO) ./notifier_builtin_replay_store -t

start2:  stop
	sed 's/[ \t]*<!-- enableBuiltinReplayStore -->/          <enabled>false<\/enabled>/' confd.conf > confd_prim.conf
	$(CONFD) -c ./confd_prim.conf $(CONFD_FLAGS)
	LD_LIBRARY_PATH=$(CONFD_SO) ./notifier -t

######################################################################
stop:
	### Stopping any confd daemon
	$(CONFD) --stop || true

######################################################################
query:
	$(CONFD_DIR)/bin/netconf-console --get -x netconf

subscribe:
	$(CONFD_DIR)/bin/netconf-console -s all sub.xml

replay:
	$(CONFD_DIR)/bin/netconf-console -s all replay.xml

subscribe-netconf:
	$(CONFD_DIR)/bin/netconf-console --create-subscription=NETCONF

edit-config1:
	$(CONFD_DIR)/bin/netconf-console --edit-config=edit1.xml

edit-config2:
	$(CONFD_DIR)/bin/netconf-console --edit-config=edit2.xml

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
