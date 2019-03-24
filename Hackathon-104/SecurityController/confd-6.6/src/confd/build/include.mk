# Make sure makefiles which have a usage target first use it even if
# they include this file at the top...

iusage: usage

# Define common ConfD build tools and flags

OSNAME       := $(shell uname -s)


CONFD        = $(CONFD_DIR)/bin/confd
CONFDC       = $(CONFD_DIR)/bin/confdc
PYANG        = $(CONFD_DIR)/bin/pyang
SMIDUMP      = $(CONFD_DIR)/bin/smidump
INCLUDE      = -I$(CONFD_DIR)/include
CONFD_LIB   ?= $(CONFD_DIR)/lib/libconfd.a
CONFD_VSN    = $(shell cat $(CONFD_DIR)/src/confd/build/vsn.mk | \
                       grep CONFDVSN | sed 's/.*=//')

## If you get irritated by the fail on warnings, set this variable
ifeq ($(shell echo $$FXS_NO_FAIL_ON_WARNING), true)
FXS_WERR     =
else
FXS_WERR     ?= --fail-on-warnings
endif

ifeq ($(OSNAME),QNX)
LIBS         = $(CONFD_LIB) -lsocket -lm
else
LIBS         = $(CONFD_LIB) -lpthread -lm
LIBDL        = -ldl
endif
CFLAGS       = -Wall -g $(INCLUDE)
CDB_DIR      = ./confd-cdb

ifeq ($(OSNAME),QNX)
KILLALL      = slay -f
else
ifeq ($(OSNAME),SunOS)
KILLALL      = pkill -x
else
KILLALL      = killall
endif
endif

JARFILE     = $(CONFD_DIR)/java/jar/conf-api-$(CONFD_VSN).jar
LOG4J       = $(CONFD_DIR)/java/jar/log4j-1.2.16.jar
CLASSPATH   = $(JARFILE):$(LOG4J):.
JAVAC       = javac

ifeq ($(OSNAME),FreeBSD)
CFLAGS      += -I/usr/local/include
LIBS        += -L/usr/local/lib
LIBDL       =
endif
ifeq ($(OSNAME),NetBSD)
CFLAGS      += -I/usr/pkg/include
LIBS        += -L/usr/pkg/lib
LIBDL       =
endif
ifeq ($(OSNAME),Darwin)
CFLAGS      += -I/opt/local/include
LIBS        += -L/opt/local/lib
endif
ifeq ($(OSNAME),QNX)
CC          = gcc
CFLAGS      += -D_POSIX_C_SOURCE=200112 -I/opt/include -I/usr/local/include -I/usr/pkg/include
LIBS        += -L/opt/lib -L/usr/local/lib -L/usr/pkg/lib
endif
ifeq ($(OSNAME),SunOS)
SHELL       = /usr/xpg4/bin/sh
MAKEARGS    = SHELL=$(SHELL)
export PATH := /opt/csw/bin:/usr/local/bin:/usr/xpg4/bin:$(PATH)
CC          = gcc
LIBS        += -lsocket -lnsl
endif

ifeq ($(OSNAME),Darwin)
SHARED_FLAGS= -dynamiclib
LD_ENV      = DYLD_LIBRARY_PATH
else
SHARED_FLAGS= -shared
LD_ENV      = LD_LIBRARY_PATH
endif

# Targets to require/reject specific OS

.PHONY: linux not_sunos

linux:
ifneq ($(OSNAME),Linux)
	@echo "This example only works on Linux" ; exit 1
endif

not_sunos:
ifeq ($(OSNAME),SunOS)
	@echo "This example does not work on Solaris" ; exit 1
endif

# Define default ConfD build rules

%.h: %.fxs
	$(CONFDC) --emit-h $*.h $*.fxs

%.java: %.fxs
	$(CONFDC) --emit-java $*.java $*.fxs

%_ns.py: %.fxs
	$(CONFDC) $(CONFDC_PYTHON_FLAGS) --emit-python $*_ns.py $*.fxs

%.fxs: %.yang
	$(CONFDC) $(FXS_WERR) $(EXTRA_LINK_FLAGS) -c -o $@  $<

%.ccl: %.cli
	$(CONFDC) -c $<

%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS)

%.class: %.java
	$(JAVAC) -classpath $(CLASSPATH) $<

%.bin: %.fxs
	$(CONFDC) -c $*.mib $*.fxs -f $(CONFD_DIR)/etc/confd/snmp

ssh-keydir:
	ln -s $(CONFD_DIR)/etc/confd/ssh $@

iclean:
	-rm -rf \
		*.o *.a *.xso *.fxs *.xsd *.ccl \
		*_proto.h \
		$(CDB_DIR) *.db aaa_cdb.* \
		rollback*/rollback{0..999} rollback{0..999} \
		cli-history \
		host.key host.cert ssh-keydir \
		*.log confderr.log.* \
		etc *.access \
		running.invalid global.data _tmp*

$(CDB_DIR):
	-mkdir -p $(CDB_DIR)
	cp $(CONFD_DIR)/var/confd/cdb/aaa_init.xml $(CDB_DIR)
