
######################################################################
# Interface Status example
# (C) 2008 Tail-f Systems
#
# See the README file for more information
######################################################################

usage:
	@echo "See README file for more instructions"
	@echo "make all	    Build all example files"
	@echo "make clean   Remove all built and intermediary files"
	@echo "make start   Start CONFD daemon and example agent"
	@echo "make stop    Stop any CONFD daemon and example agent"
	@echo "make cli	    Start the CONFD Command Line Interface"

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
CONFD_HOST=127.0.0.1

# auto-compile all the .cli files
CCL=$(patsubst %.cli,%.ccl,$(wildcard *.cli))

%.ccl: %.cli
	$(CONFDC) -c $<

all:	    example.fxs example.h twophase $(CCL) $(CDB_DIR) ssh-keydir

example.fxs: ./example.yang
	$(CONFDC) -c $< -o $@

twophase: twophase.c
	$(CC) -o $@ $< $(CFLAGS) $(LIBS)

######################################################################
clean:	      iclean
	-rm -rf $(CS).cs *.fxs *.h twophase logs/* *_tmp_twophase twophase.dSYM
	-rm -rf cli-history hardware.txt

######################################################################

hardware_up:
	@echo "1" > hardware.txt

hardware_down:
	@echo "0" > hardware.txt

start:	stop hardware_up
	: ### Start the confd daemon with our example specific confd-config
	@echo "CONFD_FLAGS= $(CONFD_FLAGS)"
	$(CONFD)  -c confd.conf $(CONFD_FLAGS)
	./twophase /system

######################################################################
stop:
	: ### Stopping any currently running confd daemon
	$(CONFD) --stop	   || true

######################################################################
cli:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive || echo Exit
cli-c:
	$(CONFD_DIR)/bin/confd_cli --user=admin --groups=admin \
		--interactive --host $(CONFD_HOST) -C || echo Exit
