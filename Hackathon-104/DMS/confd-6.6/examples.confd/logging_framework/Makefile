######################################################################
#  ConfD logging framework example (use of traceh.h)
# (C) 2017 Tail-f Systems
#
# See the README files for more information
######################################################################

usage:
	@echo "make all      Build all example files"
	@echo "make clean    Remove all built and intermediary files"
	@echo "make start    Start ConfD daemon and example agent"
	@echo "make stop     Stop any ConfD daemon and example agent"
	@echo "make cli      Start the ConfD Command Line Interface"

######################################################################
# Where is ConfD installed? Make sure CONFD_DIR points it out
#CONFD_DIR ?= ../../..

# Include standard ConfD build definitions and rules
include $(CONFD_DIR)/src/confd/build/include.mk

# In case CONFD_DIR is not set (correctly), this rule will trigger
$(CONFD_DIR)/src/confd/build/include.mk:
	@echo 'Where is ConfD installed? Set $$CONFD_DIR to point it out!'
	@echo ''
	@exit 1

######################################################################
# Example specific definitions and rules
CONFD_FXS_DIR = $(CONFD_DIR)/etc/confd
#CONFD = $(CONFD_DIR)/bin/confd
CONFD_FLAGS = --addloadpath $(CONFD_FXS_DIR)
#START_FLAGS ?=

#include path for traceh.h
CFLAGS += -I$(CONFD_DIR)/examples.confd/include
# change compile time logging level here (e.g. -DT_LOG_INFO)
CFLAGS += -DT_LOG_INFO
CONFD_FLAGS =--ignore-initial-validation --addloadpath $(CONFD_DIR)/etc/confd
START_FLAGS ?=

SRC  = logging_example.c

YANG = logging-model.yang
BIN = logging-example

OBJS = $(SRC:.c=.o)
FXS = $(YANG:.yang=.fxs)
YANG_H = $(YANG:.yang=.h)

$(BIN): $(OBJS)
	$(CC) -o $@ $(OBJS) $(LIBS)

######################################################################
all: $(FXS) $(YANG_H) $(BIN) $(CDB_DIR) ssh-keydir
	@echo "Build complete"

######################################################################
clean:	iclean
	rm -rf *.trace 2> /dev/null || true
	rm -rf cli-history 2> /dev/null || true
	rm -f $(YANG_H) $(BIN)

######################################################################
start:  stop
	$(CONFD) -c confd.conf $(CONFD_FLAGS)
	./$(BIN) &

######################################################################
stop:
    ### Killing any confd daemon or confd agents
	killall -9 $(BIN) || true
	$(CONFD) --stop   || true

######################################################################
cli:
	$(CONFD_DIR)/bin/confd_cli -C --user=admin --groups=admin \
		--interactive || echo Exit
######################################################################
