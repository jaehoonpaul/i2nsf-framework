######################################################################
# (C) 2014 Tail-f Systems
######################################################################

# Where is confd installed?
# Change here, or override with environment variable
include $(CONFD_DIR)/src/confd/build/include.mk
#CONFD_DIR ?= $(PWD)/..
#VALGRIND=valgrind --tool=memcheck --leak-check=full  --show-reachable=yes --track-origins=yes --malloc-fill=0xAA --free-fill=0xBB -v

# Set/override to sudo or equivalent
SUEXEC ?= sudo

# Specify selected features
#LINUXCFG_FEATURES = LINUXCFG_SYSTEM,LINUXCFG_IPMIBS,LINUXCFG_INTERFACES
# LINUXCFG_INTERFACES
LINUXCFG_FEATURES = LINUXCFG_SYSTEM,LINUXCFG_INTERFACES,LINUXCFG_IPMIBS,\
	LINUXCFG_IP,LINUXCFG_ROUTING

# Specify option parameters to be passed to linuxcfg during start
LINUXCFG_OPTIONS ?=-i

# Specify directory where to create linuxcfg.log
LINUXCFG_LOG_DIR ?=.

# Turn ON/OFF the components based on LINUXCFG_FEATURES setting
COMPONENTS =

ifneq (,$(findstring LINUXCFG_SYSTEM, $(LINUXCFG_FEATURES)))
COMPONENTS += ietf_system
endif

ifneq (,$(findstring LINUXCFG_IPMIBS, $(LINUXCFG_FEATURES)))
COMPONENTS += ipmibs
endif

ifneq (,$(findstring LINUXCFG_INTERFACES, $(LINUXCFG_FEATURES)))
COMPONENTS += ietf_interfaces
endif

ifneq (,$(findstring LINUXCFG_IP, $(LINUXCFG_FEATURES)))
COMPONENTS += ietf_ip
endif

ifneq (,$(findstring LINUXCFG_ROUTING, $(LINUXCFG_FEATURES)))
COMPONENTS += ietf_routing
endif


# Pass LINUXCFG_FEATURES as macros to the compiler.
# $(CFLAGS_LINUXCFG_FEATURES) will be added to $(CFLAGS) in include.mk
ifneq (,$(LINUXCFG_FEATURES))
comma:=,
def:=-D
CFLAGS_LINUXCFG_FEATURES:=$(def)$(subst $(comma),\
	 $(def),$(LINUXCFG_FEATURES))

export CFLAGS_LINUXCFG_FEATURES
endif

export CONFD_DIR

include include.mk

#CONFD     = $(CONFD_DIR)/bin/confd

INCLUDE   = -I$(CONFD_DIR)/include -I.
LIBDIR    = -L$(CONFD_DIR)/lib

PREFIX_INSTALLDIR ?= .install
ifeq ($(PREFIX_INSTALLDIR),$(CONFD_DIR))
BIN_INSTALLDIR    ?= $(CONFD_DIR)/bin
SCRIPT_INSTALLDIR ?= $(BIN_INSTALLDIR)
CONFD_INSTALLDIR  ?= $(CONFD_DIR)/etc/confd
INIT_INSTALLDIR   ?= $(CONFD_DIR)/var/confd/cdb
else
BIN_INSTALLDIR    = $(PREFIX_INSTALLDIR)/confd
SCRIPT_INSTALLDIR = $(BIN_INSTALLDIR)
CONFD_INSTALLDIR  = $(PREFIX_INSTALLDIR)/confd
INIT_INSTALLDIR   = $(CONFD_INSTALLDIR)/cdb
endif

NS_PREFIX = NONE

SUBDIRS   = $(COMPONENTS)
COMPLIBS  = $(foreach d, $(COMPONENTS), $(d)/$(d).a)
COMPDEFS  = $(COMPONENTS:%=-DWITH_%)

SRC	  = linuxcfg.c components.c linuxcfg_util.c
OBJS	  = $(SRC:.c=.o)
LIBS	  = $(LIBDIR) -lconfd -lpthread

echo:
	@echo "\n--------------------------------------------------------------"
	@echo "[linuxcfg] Current setup:"
	@echo "[linuxcfg] LINUXCFG_FEATURES = $(LINUXCFG_FEATURES)"
	@echo "[linuxcfg] COMPONENTS = $(COMPONENTS)"
	@echo "--------------------------------------------------------------\n"

#all:	$(SUBDIRS) execd execd.fxs $(CONFD_DIR)/etc/confd/aaa_cdb.fxs
all:	echo $(SUBDIRS:=-fxs) $(SUBDIRS) linuxcfg

ifdef LINUXCFG_INIT
all:	linuxcfg_init.xml
endif

linuxcfg: $(OBJS) $(COMPLIBS)
	$(CC) -o $@ $(OBJS) $(COMPLIBS) $(LIBS) $(LDFLAGS)

%.fxs: %.yang
	$(CONFDC) -c -o $@ $<

components.o: components.c linuxcfg_api.h Makefile
	$(CC) -c $(CFLAGS) $(COMPDEFS) $<

linuxcfg.o: linuxcfg_api.h

linuxcfg_init.xml:
	./mk_linuxcfg_init $(COMPONENTS) > linuxcfg_init.xml

%-fxs:
	$(MAKE) -C $* fxs

$(SUBDIRS):
	$(MAKE) -C $@

.PHONY: all $(SUBDIRS)

CONFDIRS = candidate cdb rollback ssh state
CONFDFXS = ietf-inet-types ietf-yang-types iana-crypt-hash

install: all install_fxs
ifneq ($(PREFIX_INSTALLDIR),$(CONFD_DIR))
	@mkdir -p $(addprefix $(CONFD_INSTALLDIR)/, $(CONFDIRS))
	@install -m 0644 confd.conf $(CONFD_INSTALLDIR)
	@mkdir -p $(CONFD_INSTALLDIR)/log
	@echo now ssh: $(CONFD_DIR) and $(CONFD_INSTALLDIR).
	@install -m 0644 $(CONFD_DIR)/etc/confd/ssh/* \
			$(CONFD_INSTALLDIR)/ssh
ifeq (,$(wildcard $(CONFD_DIR)/etc/confd/tailf-aaa.fxs))
	@install -m 0644 $(CONFD_DIR)/etc/confd/aaa_cdb.fxs \
		$(CONFD_INSTALLDIR)
else
	@install -m 0644 $(CONFD_DIR)/etc/confd/tailf-aaa.fxs\
		$(CONFD_INSTALLDIR)
	@install -m 0644 $(CONFD_DIR)/etc/confd/tailf-acm.fxs\
		$(CONFD_INSTALLDIR)
	@install -m 0644 $(CONFD_DIR)/etc/confd/ietf-netconf-acm.fxs\
		$(CONFD_INSTALLDIR)
endif
	@install -m 0644 $(CONFDFXS:%=$(CONFD_DIR)/etc/confd/%.fxs)\
		$(CONFD_INSTALLDIR)
	@install -m 0644 $(CONFD_DIR)/var/confd/cdb/aaa_init.xml\
		$(CONFD_INSTALLDIR)/cdb

endif

	@echo "\nInstalling linuxcfg"
	@install -m 0755 -s linuxcfg $(BIN_INSTALLDIR)

ifdef LINUXCFG_INIT
	@install -m 0644 linuxcfg_init.xml $(INIT_INSTALLDIR)
endif
	@for d in $(SUBDIRS); do \
	echo "Installing module: $$d"; \
	$(MAKE) CONFD_INSTALLDIR=$(abspath $(CONFD_INSTALLDIR))\
		-C $$d install; done

install_fxs: all
ifeq ($(CONFD_INSTALLDIR),)
	@echo ''
	@echo "CONFD_INSTALLDIR is not specified!"
	@exit 1
endif
	@echo "\nInstalling *.fxs files to $(CONFD_INSTALLDIR):\n"
#	install -m 0644 linuxcfg.fxs $(CONFD_INSTALLDIR)
	@for d in $(SUBDIRS); do \
		$(MAKE) CONFD_INSTALLDIR=$(CONFD_INSTALLDIR)\
			KEEP_EXISTING=$(KEEP_EXISTING) \
			-C $$d install_fxs; \
	done

install_init_xml:
ifeq ($(INIT_XML_INSTALLDIR),)
	@echo ''
	@echo "INIT_XML_INSTALLDIR is not specified!"
	@exit 1
endif
	@echo "Installing *_init.xml files to $(INIT_XML_INSTALLDIR) :"
ifdef LINUXCFG_INIT
	@install -m 0644 linuxcfg_init.xml $(INIT_XML_INSTALLDIR)
endif
	for d in $(SUBDIRS); do $(MAKE)\
		INIT_XML_INSTALLDIR=$(INIT_XML_INSTALLDIR) \
		-C $$d install_init_xml; done

clean: clean_subdirs
ifneq ($(PREFIX_INSTALLDIR),$(CONFD_DIR))
	-rm -rf $(PREFIX_INSTALLDIR)
endif
	-rm -rf *~ *.o *.a linuxcfg.fxs linuxcfg linuxcfg.h \
		linuxcfg_init.xml 2> /dev/null
	-rm -rf *log *trace cli-history 2> /dev/null || true

clean_subdirs:
	-rm $(LINUXCFG_LOG_DIR)/linuxcfg.log
	-for d in $(SUBDIRS); do $(MAKE) PREFIX_INSTALLDIR=$(PREFIX_INSTALLDIR) \
	 -C $$d clean; done

# 'make local_install' and 'make local_start'
# will create and use 'confd' subdirectory here
local_%:
	$(MAKE) PREFIX_INSTALLDIR=. $*

start: install stop
	@echo "\nStarting phase 0."
	@(cd $(CONFD_INSTALLDIR); confd \
		--addloadpath $(CONFD_DIR)/etc/confd/snmp \
		-c confd.conf --start-phase0)
	confd --wait-phase0

	@echo "Starting linuxcfg."
	@$(MAKE) start_linuxcfg

	confd --start-phase1
	confd --start-phase2

stop: stop_linuxcfg
	-confd --stop

start_linuxcfg:
ifeq ($(BIN_INSTALLDIR),)
	@echo ''
	@echo "BIN_INSTALLDIR is not specified!"
	@exit 1
endif
	$(SUEXEC) LINUXCFG_DIR=$(LINUXCFG_DIR) \
		  LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) \
	$(VALGRIND) $(BIN_INSTALLDIR)/linuxcfg $(LINUXCFG_OPTIONS) \
	      > $(LINUXCFG_LOG_DIR)/linuxcfg.log 2>&1

stop_linuxcfg:
	@-$(SUEXEC) pkill linuxcfg

debug_linuxcfg: linuxcfg stop_linuxcfg

	$(MAKE) LINUXCFG_OPTIONS=-Dtl start_linuxcfg\
		LINUXCFG_BINDIR=$(LINUXCFG_BINDIR)

cli: all
	$(CONFD_DIR)/bin/confd_cli -C --user=admin --groups=admin \
		--interactive || echo Exit
