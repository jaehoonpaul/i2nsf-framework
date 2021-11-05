######################################################################
# ConfD MAAPI example
# (C) 2006 Tail-f Systems
#
# See the README file for more information
######################################################################

usage:
	@echo "See README file for more instructions"
	@echo "make all      Build all example files"
	@echo "make clean    Remove all built and intermediary files"
	@echo "make start    Start ConfD daemon"
	@echo "make stop     Stop any ConfD daemon"
	@echo "make cli      Start the ConfD Command Line Interface"
	@echo "make demo     Run demo application"


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

######################################################################
# Example specific definitions and rules

SRC       = type_demo.c confd_type.c
HDR       = types.h
OBJS      = $(SRC:.c=.o)
CFLAGS    += -fPIC

all:	types.fxs confd_type.so type_demo $(CDB_DIR) ssh-keydir
	@echo "Build complete"

$(OBJS): $(SRC) $(HDR)

type_demo: $(OBJS)
	$(CC) $(OBJS) $(LIBS) -o $@

confd_type.so: confd_type.o
	$(CC) $(SHARED_FLAGS) $< -o $@


######################################################################
clean:	iclean
	-rm -rf types.h type_demo confd_type.so 2> /dev/null || true

start:  stop $(CDB_DIR)
	$(LD_ENV)=$(CONFD_DIR)/lib $(CONFD) -c ./confd.conf $(CONFD_FLAGS)

######################################################################
stop:
	### Stopping any confd daemon
	$(CONFD) --stop || true


######################################################################
cli:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit

######################################################################
cli-c:
	$(CONFD_DIR)/bin/confd_cli -C --user=admin --groups=admin \
		--interactive || echo Exit

######################################################################
demo:
	./type_demo

######################################################################
