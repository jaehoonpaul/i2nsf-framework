######################################################################
# CDB Upgrade Simple example
# (C) 2006 Tail-f Systems
#
# See the README file for more information
######################################################################

usage:
	@echo "See README file for more instructions"
	@echo "make all      Build all example files"
	@echo "make clean    Remove all built and intermediary files"
	@echo "make start    Start ConfD with package v1"
	@echo "make pkg_v2   Build and install package v2"
	@echo "make stop     Stop the ConfD daemon"
	@echo "make query    Run query against ConfD"
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

######################################################################
# Example specific definitions and rules

CONFD_FXS_DIR   = $(CONFD_DIR)/etc/confd
CONFD_FXS_FILES = $(wildcard $(CONFD_FXS_DIR)/*.fxs)

CONFD_FLAGS = --addloadpath $(CONFD_FXS_DIR)

V1_FILES = pkg/v1/simple.fxs pkg/v1/confd_type.so \
	   pkg/v1/commands-j.ccl pkg/v1/commands-c.ccl
V2_FILES = $(V1_FILES:pkg/v1/%=pkg/v2/%) \
	   pkg/v2/version.sh pkg/v2/new.fxs pkg/v2/new_init.xml
PKG_V1   = pkg/v1 $(V1_FILES)
PKG_V2   = pkg/v2 $(V2_FILES)

CFLAGS  += -DCONFD_FXS_DIR="\"$(CONFD_FXS_DIR)\"" \
	   -fPIC

all:	$(CDB_DIR) ssh-keydir upgrade cdb_subscriber pkg_v1 pkg/current
	@echo "Build complete"

pkg_v1: $(PKG_V1)

pkg_v2: $(PKG_V2)

upgrade: upgrade.o
	$(CC) -o $@ $< $(LIBS)

cdb_subscriber: cdb_subscriber.o
	$(CC) -o $@ $< $(LIBS)

pkg/%.fxs: src/%.yang
	$(CONFDC) --fail-on-warnings --yangpath `dirname $<` \
		--yangpath `dirname $@` -c -o $@ $<

pkg/%.yang: src/%.mib
	$(CONFDC) --mib2yang -o $@ $<

pkg/%.bin: src/%.mib
	$(CONFDC) -c -o $@ $<

pkg/%.so: src/%.o
	$(CC) $(SHARED_FLAGS) $< -o $@

pkg/%.ccl: src/%.cli
	$(CONFDC) -c -o $@ $<

pkg/%.sh: src/%.sh
	cp $< $@
	chmod +x $@

pkg/%_init.xml: src/%_init.xml
	cp $< $@

pkg/v1 pkg/v2:
	mkdir -p $@

pkg/current:
	rm -f $@
	ln -s v1 $@

######################################################################
clean: iclean
	-rm -f confd_candidate.db upgrade cdb_subscriber localhost.access
	-rm -rf pkg/*
	-rm -rf *log cli-history 2> /dev/null || true

######################################################################
start:  stop $(CDB_DIR) pkg/current/simple.fxs
	: ### Start the confd daemon using simple.fxs
	@if [ -f $(CDB_DIR)/A.cdb ]; then		\
		rm -f $(CDB_DIR)/*_init.xml;		\
	else						\
		cp *_init.xml $(CDB_DIR)/;	 	\
	fi
	$(LD_ENV)=$(CONFD_DIR)/lib $(CONFD) -c confd.conf $(CONFD_FLAGS)
	CONFD_DIR=$(CONFD_DIR) sh ./setup_oper_data.sh
	./cdb_subscriber

######################################################################
stop:
	: ### Stopping any confd daemon running
	$(CONFD) --stop    || true

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
	$(CONFD_DIR)/bin/netconf-console -snoaaa --get-config --with-defaults=report-all-tagged

######################################################################
