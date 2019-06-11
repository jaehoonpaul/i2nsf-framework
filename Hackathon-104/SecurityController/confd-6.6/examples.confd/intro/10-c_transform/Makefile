######################################################################
# Transform example
# (C) 2017 Tail-f Systems
#
# See the README file for more information
######################################################################

usage:
	@echo "See README file for more instructions"
	@echo "make all      Build all example files"
	@echo "make clean    Remove all built and intermediary files"
	@echo "make start    Start ConfD daemon and example agent"
	@echo "make stop     Stop any ConfD daemon and example agent"
	@echo "make query    Run NETCONF query against ConfD"
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

print-%  : ; @echo $* = $($*)

CONFD_FLAGS = --addloadpath $(CONFD_DIR)/etc/confd --addloadpath $(BUILD_DIR)/
START_FLAGS ?=

CINCLUDE_DIRS += $(CONFD_DIR)/include
CINCLUDE_DIRS += $(CONFD_DIR)/examples.confd/include/
CINCLUDE_DIRS += $(BUILD_DIR)/

CFLAGS += -DT_LOG_TRACE

BUILD_DIR = ./build
YANG_DIR = ./yangs
SRC_DIR = ./src

#VALGRIND = valgrind --tool=memcheck --leak-check=full --show-reachable=no\
 --track-origins=yes -v
DAEMON_BINARY = user-storage-trans-app

YANGS = $(wildcard $(YANG_DIR)/*.yang)
FXSS = $(patsubst $(YANG_DIR)/%.yang, $(BUILD_DIR)/%.fxs, $(YANGS))
HSS = $(FXSS:.fxs=.h)

SRCS = $(wildcard $(SRC_DIR)/*.c)
OBJS = $(patsubst $(SRC_DIR)/%.c, $(BUILD_DIR)/%.o, $(SRCS))

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/%.fxs: $(YANG_DIR)/%.yang
	$(CONFDC) -o $@ -c $<

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) -o $@ -c $< $(CFLAGS) $(addprefix -I, $(CINCLUDE_DIRS))

$(DAEMON_BINARY): $(HSS) $(OBJS)
	$(CC) -o $(BUILD_DIR)/$@ $(OBJS) $(LIBS)

all:	$(BUILD_DIR) $(FXSS) $(DAEMON_BINARY) $(CDB_DIR) ssh-keydir
	@echo "Build complete"

## need this particular vsn of aaa_init.xml for this example
#$(CDB_DIR)/aaa_init.xml: aaa_init.xml
#	-mkdir -p $(CDB_DIR)
#	cp $< $@

######################################################################
DIRS_TO_CLEAN = $(BUILD_DIR) cli-history/

clean:	iclean
	-rm -rf $(DIRS_TO_CLEAN) 2> /dev/null || true

start:  stop
	$(CONFD) -c confd.conf $(CONFD_FLAGS)
	$(VALGRIND) $(BUILD_DIR)/$(DAEMON_BINARY) 2>&1 | \
	    tee $(DAEMON_BINARY).log $(START_FLAGS) &

######################################################################
stop:
	$(CONFD) --stop    || true
	$(KILLALL) $(DAEMON_BINARY) || true

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
	$(CONFD_DIR)/bin/netconf-console-tcp -u admin --get-config

######################################################################
