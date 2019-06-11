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

MSRC       = maapi.c 
MOBJS      = $(MSRC:.c=.o)

CSRC       = cdb_listener.c 
COBJS      = $(CSRC:.c=.o)

######################################################################
all:	maapi cdb_listener
	@echo "Build complete"

$(MOBJS):	$(MSRC)
$(COBJS):	$(CSRC)

maapi:	$(MOBJS)
	 $(CC) $(MOBJS) $(LIBS) -o $@


cdb_listener:	$(COBJS)
	 $(CC) $(COBJS) $(LIBS) -o $@


######################################################################
clean:	
	-rm -rf maapi cdb_listener *.o 2> /dev/null || true

